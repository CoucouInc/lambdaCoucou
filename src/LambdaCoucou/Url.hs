{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Url where

import           Control.Monad
import qualified Control.Monad.Except      as Ex
import           Control.Monad.IO.Class
import qualified Data.ByteString           as BS
import           Data.Text                 (Text)
import qualified Data.Text                 as Tx
import qualified Data.Text.Encoding        as Tx.Enc
import qualified Data.Text.Encoding.Error  as Tx.Enc
import qualified Network.HTTP.Client       as HTTP.C
import qualified Network.HTTP.Req          as Req
import           Network.HTTP.Req          ((/:), (=:))
import qualified Network.HTTP.Types.Status as HTTP.Status
import qualified Network.HTTP.Types.URI    as HTTP.Uri
import qualified Text.HTML.TagSoup         as HTML
import qualified Data.List as List
import Control.Lens ((^?))
import qualified Data.Aeson.Lens as AL

import qualified LambdaCoucou.State as LC.St

fetchUrlData :: MonadIO m => LC.St.YoutubeAPIKey -> Text -> m (Either FetchError Text)
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
  UnparsableUrl
    -> "Could not parse url: " <> textUrl
  HttpExc exc
    -> "Http exception: " <> showHttpException exc <> " for url: " <> textUrl
  InvalidContentType ct
    -> "Invalid content type: " <> Tx.Enc.decodeUtf8 ct <> " for url: " <> textUrl
  DecodingError _
    -> "UTF8 decoding error for url: " <> textUrl
  TitleNotFound
    -> "Could not find <title> tag for url: " <> textUrl
  YoutubeIdNotFound
    -> "Could not find the youtube video's id for url: " <> textUrl


showHttpException :: Req.HttpException -> Text
showHttpException = \case
  Req.JsonHttpException str -> Tx.pack str
  Req.VanillaHttpException exc -> case exc of
    HTTP.C.InvalidUrlException _url reason -> Tx.pack reason
    HTTP.C.HttpExceptionRequest _req content -> case content of
      HTTP.C.StatusCodeException rsp _body
        -> "Invalid status code ("
        <> Tx.pack (show $ HTTP.Status.statusCode $ HTTP.C.responseStatus rsp)
        <> ")"
      HTTP.C.TooManyRedirects _ -> "too many redirects"
      HTTP.C.ResponseTimeout -> "response timeout"
      HTTP.C.ConnectionTimeout -> "connection timeout"
      HTTP.C.ConnectionFailure _ -> "connection failure"
      other -> "weird HTTP error: " <> Tx.pack (show other)

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
    Just x -> pure x

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
    Right x -> pure x

  maybe (Ex.throwError TitleNotFound) pure (parseTitle body)


parseTitle :: Text -> Maybe Text
parseTitle body =
  let tags = HTML.parseTags body
      mbTitle = tail $ dropWhile (HTML.~/= HTML.TagOpen ("title" :: Text) []) tags
  in case mbTitle of
       [] -> Nothing
       (title:_) -> Just (HTML.fromTagText title)



fetchYtUrlData
  :: ( MonadIO m
     , Req.MonadHttp m
     , Ex.MonadError FetchError m
     )
  => LC.St.YoutubeAPIKey
  -> Text
  -> m Text
fetchYtUrlData ytApiKey textUrl = do
  -- 0x3f is the character '?'
  let (_, query) = BS.break (== 0x3f) (Tx.Enc.encodeUtf8 textUrl)
  ytId <- case List.lookup "v" (HTTP.Uri.parseQueryText query) of
    Nothing -> Ex.throwError YoutubeIdNotFound
    Just (Just x) -> pure x
    Just _ -> Ex.throwError YoutubeIdNotFound

  bsResp <- either Ex.throwError (pure . Req.responseBody) =<< runFetch (Req.req Req.GET
    (Req.https "www.googleapis.com" /: "youtube" /: "v3" /: "videos")
    Req.NoReqBody
    Req.bsResponse
    ( "part" =: ("snippet" :: Text)
    <> "id" =: ytId
    <> "key" =: LC.St.getYoutubeAPIKey ytApiKey
    ))

  let mbTitle = bsResp ^? AL.key "items" . AL.nth 0 . AL.key "snippet" . AL.key "title" . AL._String
  case mbTitle of
    Nothing -> Ex.throwError TitleNotFound
    Just title -> pure title
