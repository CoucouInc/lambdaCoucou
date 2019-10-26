{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeApplications           #-}

module LambdaCoucou.Url where

import           Control.Applicative
import           Control.Lens                 ((%=), (^?))
import qualified Control.Monad.Except         as Ex
import           Control.Monad.IO.Class
import qualified Control.Monad.State.Strict   as St
import qualified Data.Aeson.Lens              as AL
import qualified Data.ByteString              as BS
import           Data.Generics.Product.Fields (field)
import qualified Data.List                    as List
import           Data.Text                    (Text)
import qualified Data.Text                    as Tx
import qualified Data.Text.Encoding           as Tx.Enc
import qualified Data.Text.Encoding.Error     as Tx.Enc
import           Data.Void
import           Network.HTTP.Req             ((/:), (=:))
import qualified Network.HTTP.Req             as Req
import qualified Network.HTTP.Types.URI       as HTTP.Uri
import qualified Network.IRC.Client           as IRC.C
import qualified Network.IRC.Client.Events    as IRC.Ev
import qualified Text.HTML.TagSoup            as HTML
import qualified Text.Megaparsec              as M
import qualified Text.Megaparsec.Char         as C

import qualified LambdaCoucou.HandlerUtils    as LC.Hdl
import qualified LambdaCoucou.Http            as LC.Http
import qualified LambdaCoucou.ParserUtils     as LC.P
import qualified LambdaCoucou.State           as LC.St

fetchUrlCommandHandler
  :: Maybe Text
  -> IRC.C.IRC LC.St.CoucouState (Maybe Text)

fetchUrlCommandHandler target = do
  st <- St.get
  case LC.St.csLastUrl st of
    Nothing -> pure Nothing
    Just url ->
      Just . LC.Hdl.addTarget target . either
        (showFetchError url)
        (\title -> title <> " [" <> url <> "]")
      <$> fetchUrlData (LC.St.csYtAPIKey st) url

updateLastUrlHandler :: IRC.Ev.EventHandler LC.St.CoucouState
updateLastUrlHandler = IRC.Ev.EventHandler
  (IRC.Ev.matchType IRC.Ev._Privmsg)
  (\source (_target, raw) -> case (source, raw) of
    (IRC.Ev.Channel _ _, Right msg) -> updateLastUrl msg
    _ -> pure ()
  )

updateLastUrl :: (St.MonadState LC.St.CoucouState m) => Text -> m ()
updateLastUrl msg = field @"csLastUrl" %= (parseUrl msg <|>)

parseUrl :: Text -> Maybe Text
parseUrl = hush . M.parse urlParser ""

hush :: Either e a -> Maybe a
hush = \case
  Left _ -> Nothing
  Right x -> Just x


type Parser = M.Parsec Void Text

urlParser :: Parser Text
urlParser
  = (M.eof *> fail "eof url")
  <|> (LC.P.spaces *> (M.try urlParser' <|> LC.P.utf8Word *> urlParser))

urlParser' :: Parser Text
urlParser' = do
  proto <- C.string' "http://" <|> C.string' "https://"
  rest <- LC.P.utf8Word'
  pure $ proto <> rest


fetchUrlData
  :: MonadIO m
  => LC.St.YoutubeAPIKey
  -> Text
  -> m (Either FetchError Text)

fetchUrlData ytApiKey rawUrl =
  if isYoutube rawUrl
    then runFetch $ fetchYtUrlData ytApiKey rawUrl
    else runFetch $ fetchGeneralUrlData rawUrl


data FetchError
  = UnparsableUrl
  | HttpExc Req.HttpException
  | InvalidContentType BS.ByteString
  | DecodingError Tx.Enc.UnicodeException
  | TitleNotFound
  | YoutubeIdNotFound
  deriving (Show)

showFetchError :: Text -> FetchError -> Text
showFetchError textUrl = \case
  UnparsableUrl ->
    "Could not parse url: " <> textUrl
  HttpExc exc
    -> "Http exception: " <> LC.Http.showHttpException exc <> " for url: " <> textUrl
  InvalidContentType ct ->
    "Invalid content type: " <> Tx.Enc.decodeUtf8 ct <> " for url: " <> textUrl
  DecodingError _ ->
    "UTF8 decoding error for url: " <> textUrl
  TitleNotFound ->
    "Could not find <title> tag for url: " <> textUrl
  YoutubeIdNotFound ->
    "Could not find the youtube video's id for url: " <> textUrl


newtype ReqMonad m a = ReqMonad { runReqMonad :: Ex.ExceptT FetchError m a }
  deriving newtype (Functor, Applicative, Monad, Ex.MonadError FetchError, MonadIO)

instance (MonadIO m) => Req.MonadHttp (ReqMonad m) where
  handleHttpException = Ex.throwError . HttpExc

runFetch :: (MonadIO m) => ReqMonad m a -> m (Either FetchError a)
runFetch = Ex.runExceptT . runReqMonad


isYoutube :: Text -> Bool
isYoutube rawUrl
  = any (`Tx.isInfixOf` rawUrl)
    [ "https://youtube.com"
    , "https://www.youtube.com"
    , "https://youtu.be"
    , "https://www.youtu.be"
    ]


fetchGeneralUrlData
  :: ( MonadIO m
     , Req.MonadHttp m
     , Ex.MonadError FetchError m
     )
  => Text
  -> m Text

fetchGeneralUrlData textUrl = do
  parsedUrl <- case Req.parseUrl (Tx.Enc.encodeUtf8 textUrl) of
    Nothing -> Ex.throwError UnparsableUrl
    Just x  -> pure x

  -- if invalid status code, an exception will be throw anyway
  resp <- case parsedUrl of
    (Left (url, options)) ->
      Req.req Req.GET url Req.NoReqBody Req.bsResponse options
    (Right (url, options)) ->
      Req.req Req.GET url Req.NoReqBody Req.bsResponse options

  case Req.responseHeader resp "Content-Type" of
    Nothing -> pure ()
    Just ct ->
      let r | BS.isInfixOf "text" ct = pure ()
            | BS.isInfixOf "html" ct = pure ()
            | otherwise = Ex.throwError (InvalidContentType ct)
      in r

  body <- case Tx.Enc.decodeUtf8' (Req.responseBody resp) of
    Left err -> Ex.throwError (DecodingError err)
    Right x  -> pure x

  maybe (Ex.throwError TitleNotFound) pure (parseTitle body)


parseTitle :: Text -> Maybe Text
parseTitle body =
  let tags = HTML.parseTags body
      mbTitle = dropWhile (not . isTitleTag) tags
  in case mbTitle of
       []          -> Nothing
       [_]         -> Nothing
       (_:title:_) -> Just (HTML.fromTagText title)


isTitleTag :: HTML.Tag Text -> Bool
isTitleTag tag = HTML.isTagOpenName "title" tag || HTML.isTagOpenName "TITLE" tag


fetchYtUrlData
  :: ( MonadIO m
     , Req.MonadHttp m
     , Ex.MonadError FetchError m
     )
  => LC.St.YoutubeAPIKey
  -> Text
  -> m Text
fetchYtUrlData ytApiKey textUrl = do
  ytId <- case parseYoutubeIdLong textUrl <|> parseYoutubeIdShort textUrl of
    Just x -> pure x
    Nothing -> Ex.throwError YoutubeIdNotFound

  bsResp <- either Ex.throwError (pure . Req.responseBody) =<< runFetch
    (Req.req Req.GET
      (Req.https "www.googleapis.com" /: "youtube" /: "v3" /: "videos")
      Req.NoReqBody
      Req.bsResponse
      ( "part" =: ("snippet" :: Text)
      <> "id" =: ytId
      <> "key" =: LC.St.getYoutubeAPIKey ytApiKey
      )
    )

  let mbTitle = bsResp ^? AL.key "items" . AL.nth 0 . AL.key "snippet" . AL.key "title" . AL._String
  case mbTitle of
    Nothing    -> Ex.throwError TitleNotFound
    Just title -> pure title

parseYoutubeIdLong :: Text -> Maybe Text
parseYoutubeIdLong textUrl =
  -- 0x3f is the character '?'
  let (_, query) = BS.break (== 0x3f) (Tx.Enc.encodeUtf8 textUrl)
  in case List.lookup "v" (HTTP.Uri.parseQueryText query) of
    Nothing       -> Nothing
    Just (Just x) -> pure x
    Just _        -> Nothing

parseYoutubeIdShort :: Text -> Maybe Text
parseYoutubeIdShort textUrl =
  let withoutProto = Tx.drop (Tx.length "https://") textUrl
      (_, pathAndQuery) = Tx.break (== '/') withoutProto
      path = Tx.takeWhile (/= '?') (Tx.tail pathAndQuery)

  in if "youtu.be" `Tx.isInfixOf` textUrl
      then if Tx.null path then Nothing else Just path
      else Nothing
