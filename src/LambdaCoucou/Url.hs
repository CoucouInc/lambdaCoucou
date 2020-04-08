module LambdaCoucou.Url where

import qualified Control.Monad.Except as Ex
import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import qualified Data.RingBuffer as RB
import qualified LambdaCoucou.HandlerUtils as LC.Hdl
import qualified LambdaCoucou.Http as LC.Http
import qualified LambdaCoucou.ParserUtils as LC.P
import qualified LambdaCoucou.State as LC.St
import Network.HTTP.Req ((/:), (=:))
import qualified Network.HTTP.Req as Req
import qualified Network.HTTP.Types.URI as HTTP.Uri
import qualified Network.IRC.Client as IRC.C
import qualified Network.IRC.Client.Events as IRC.Ev
import RIO
import qualified RIO.ByteString as B
import qualified RIO.Map as Map
import qualified RIO.State as St
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T'
import RIO.Vector ((!?))
import System.IO (putStrLn)
import qualified Text.HTML.TagSoup as HTML
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C

fetchUrlCommandHandler ::
  LC.St.ChannelName ->
  Int ->
  Maybe Text ->
  IRC.C.IRC LC.St.CoucouState (Maybe Text)
fetchUrlCommandHandler chanName offset target = do
  state <- St.get
  mbLastUrl <- case Map.lookup chanName (LC.St.csChannels state) of
    Nothing -> pure Nothing
    Just channel -> liftIO $ RB.latest (LC.St.cstLastUrls channel) offset

  case mbLastUrl of
    Nothing -> pure $ Just $ "No URL found for offset " <> T.pack (show offset)
    Just url ->
      Just . LC.Hdl.addTarget target
        . either
          (showFetchError url)
          (\title -> title <> " [" <> url <> "]")
        <$> fetchUrlData (LC.St.csYtAPIKey state) url

updateLastUrlHandler :: IRC.Ev.EventHandler LC.St.CoucouState
updateLastUrlHandler =
  IRC.Ev.EventHandler
    (IRC.Ev.matchType IRC.Ev._Privmsg)
    ( \source (_target, raw) -> case (source, raw) of
        (IRC.Ev.Channel chanName _, Right msg) -> updateLastUrl (LC.St.ChannelName chanName) msg
        _ -> pure ()
    )

updateLastUrl :: (MonadIO m, St.MonadState LC.St.CoucouState m) => LC.St.ChannelName -> Text -> m ()
updateLastUrl chanName msg = Map.lookup chanName <$> St.gets LC.St.csChannels >>= \case
  Nothing -> pure ()
  Just chanState -> case parseUrl msg of
    Nothing -> pure ()
    Just url -> liftIO $ RB.append url (LC.St.cstLastUrls chanState)

parseUrl :: Text -> Maybe Text
parseUrl = hush . M.parse urlParser ""

hush :: Either e a -> Maybe a
hush = \case
  Left _ -> Nothing
  Right x -> Just x

type Parser = M.Parsec Void Text

urlParser :: Parser Text
urlParser =
  (M.eof *> fail "eof url")
    <|> (LC.P.spaces *> (M.try urlParser' <|> LC.P.utf8Word *> urlParser))

urlParser' :: Parser Text
urlParser' = do
  proto <- C.string' "http://" <|> C.string' "https://"
  rest <- LC.P.utf8Word'
  pure $ proto <> rest

fetchUrlData ::
  MonadIO m =>
  LC.St.YoutubeAPIKey ->
  Text ->
  m (Either FetchError Text)
fetchUrlData ytApiKey rawUrl =
  if isYoutube rawUrl
    then runFetch $ fetchYtUrlData ytApiKey rawUrl
    -- then LC.Http.req $ fetchYtUrlData ytApiKey rawUrl
    else runFetch $ fetchGeneralUrlData rawUrl

data FetchError
  = UnparsableUrl
  | HttpExc Req.HttpException
  | InvalidContentType ByteString
  | DecodingError UnicodeException
  | TitleNotFound
  | YoutubeIdNotFound
  deriving (Show)

showFetchError :: Text -> FetchError -> Text
showFetchError textUrl = \case
  UnparsableUrl ->
    "Could not parse url: " <> textUrl
  HttpExc exc ->
    "Http exception: " <> LC.Http.showHttpException exc <> " for url: " <> textUrl
  InvalidContentType ct ->
    -- the content type is coming from an http header, as a bytestring, alreadyd decoded as utf8
    let Right decoded = T.decodeUtf8' ct
     in "Invalid content type: " <> decoded <> " for url: " <> textUrl
  DecodingError _ ->
    "UTF8 decoding error for url: " <> textUrl
  TitleNotFound ->
    "Could not find <title> tag for url: " <> textUrl
  YoutubeIdNotFound ->
    "Could not find the youtube video's id for url: " <> textUrl

-- TODO: rework that to simply throw exceptions
newtype ReqMonad m a = ReqMonad {runReqMonad :: Ex.ExceptT FetchError m a}
  deriving newtype (Functor, Applicative, Monad, Ex.MonadError FetchError, MonadIO)

instance (MonadIO m) => Req.MonadHttp (ReqMonad m) where
  handleHttpException = Ex.throwError . HttpExc

runFetch :: (MonadIO m) => ReqMonad m a -> m (Either FetchError a)
runFetch = Ex.runExceptT . runReqMonad

isYoutube :: Text -> Bool
isYoutube rawUrl =
  any
    (`T.isInfixOf` rawUrl)
    [ "https://youtube.com",
      "https://www.youtube.com",
      "https://youtu.be",
      "https://www.youtu.be"
    ]

fetchGeneralUrlData ::
  ( MonadIO m,
    Req.MonadHttp m,
    Ex.MonadError FetchError m
  ) =>
  Text ->
  m Text
fetchGeneralUrlData textUrl = do
  -- drop any fragment in the url, until the issue is resolved upstream:
  -- https://github.com/mrkkrp/req/issues/73
  parsedUrl <- case Req.parseUrl (T.encodeUtf8 $ T.takeWhile (/= '#') textUrl) of
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
      let r
            | B.isInfixOf "text" ct = pure ()
            | B.isInfixOf "html" ct = pure ()
            | otherwise = Ex.throwError (InvalidContentType ct)
       in r
  body <- case T.decodeUtf8' (Req.responseBody resp) of
    Left err -> Ex.throwError (DecodingError err)
    Right x -> pure x
  maybe (Ex.throwError TitleNotFound) pure (parseTitle body)

parseTitle :: Text -> Maybe Text
parseTitle body =
  let tags = HTML.parseTags body
      mbTitle = dropWhile (not . isTitleTag) tags
   in case mbTitle of
        [] -> Nothing
        [_] -> Nothing
        (_ : title : _) -> Just (HTML.fromTagText title)

isTitleTag :: HTML.Tag Text -> Bool
isTitleTag tag = HTML.isTagOpenName "title" tag || HTML.isTagOpenName "TITLE" tag

newtype YtVideosOverview = YtVideosOverview (Vector YtVideoSnippet)
  deriving (Show)

instance JSON.FromJSON YtVideosOverview where
  parseJSON = JSON.withObject "videoOverview" $ \o ->
    YtVideosOverview <$> o .: "items"

newtype YtVideoSnippet = YtVideoSnippet
  { yvsTitle :: Text
  }
  deriving (Show)

instance JSON.FromJSON YtVideoSnippet where
  parseJSON = JSON.withObject "overview" $ \o -> do
    snippet <- o .: "snippet"
    JSON.withObject
      "snippet"
      (\snip -> YtVideoSnippet <$> snip .: "title")
      snippet

fetchYtUrlData ::
  ( MonadIO m,
    Req.MonadHttp m,
    Ex.MonadError FetchError m
  ) =>
  LC.St.YoutubeAPIKey ->
  Text ->
  m Text
fetchYtUrlData ytApiKey textUrl = do
  ytId <- case parseYoutubeIdLong textUrl <|> parseYoutubeIdShort textUrl of
    Just x -> pure x
    Nothing -> Ex.throwError YoutubeIdNotFound
  -- (YtVideosOverview overview) <-
  bsResp <-
    either Ex.throwError (pure . Req.responseBody)
      =<< runFetch
        ( Req.req
            Req.GET
            (Req.https "www.googleapis.com" /: "youtube" /: "v3" /: "videos")
            Req.NoReqBody
            Req.bsResponse
            ( "part" =: ("snippet" :: Text)
                <> "id" =: ytId
                <> "key" =: LC.St.getYoutubeAPIKey ytApiKey
            )
        )

  B.putStr bsResp
  let (r :: Either String YtVideosOverview) = JSON.eitherDecodeStrict bsResp
  liftIO $ putStrLn $ show r
  let Right (YtVideosOverview overview) = JSON.eitherDecodeStrict bsResp
  let mbTitle = yvsTitle <$> overview !? 0
  case mbTitle of
    Nothing -> Ex.throwError TitleNotFound
    Just title -> pure title

parseYoutubeIdLong :: Text -> Maybe Text
parseYoutubeIdLong textUrl =
  -- 0x3f is the character '?'
  let (_, query) = B.break (== 0x3f) (T.encodeUtf8 textUrl)
   in case lookup "v" (HTTP.Uri.parseQueryText query) of
        Nothing -> Nothing
        Just (Just x) -> pure x
        Just _ -> Nothing

parseYoutubeIdShort :: Text -> Maybe Text
parseYoutubeIdShort textUrl =
  let withoutProto = T.drop (T.length "https://") textUrl
      (_, pathAndQuery) = T.break (== '/') withoutProto
      path = T.takeWhile (/= '?') (T'.tail pathAndQuery)
   in if "youtu.be" `T.isInfixOf` textUrl
        then if T.null path then Nothing else Just path
        else Nothing
