{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module LambdaCoucou.Url where

import qualified Control.Monad.Except as Ex
import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import qualified Data.Generics.Product.Fields as Fields
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
import Say
import System.Environment (getEnv)
import qualified Text.HTML.TagSoup as HTML
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.URI as URI

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
        (IRC.Ev.Channel chanName _, Right msg) -> do
          mbMessage <- updateLastUrl (LC.St.ChannelName chanName) msg
          traverse_ (IRC.C.send . IRC.Ev.Privmsg chanName . Right) mbMessage
        _ -> pure ()
    )

-- | if there is a URL in the given message, update the list of latest seen urls
-- also strip this URL from any tracking query param. If the URL has some
-- tracker, will return a message with the updated url
updateLastUrl :: (MonadIO m, St.MonadState LC.St.CoucouState m) => LC.St.ChannelName -> Text -> m (Maybe Text)
updateLastUrl chanName msg =
  Map.lookup chanName <$> St.gets LC.St.csChannels >>= \case
    Nothing -> pure Nothing
    Just chanState -> case parseUrlMessage msg of
      Nothing -> pure Nothing
      Just url -> do
        let url' = withoutTracker url
        liftIO $ RB.append (fromMaybe url url') (LC.St.cstLastUrls chanState)
        pure Nothing
        -- the tracker detection is completely broken, disable fully until fixed
        -- case url' of
        --   Nothing -> pure Nothing
        --   Just updatedUrl ->
        --     let response = updatedUrl <> " <- cette URL n'a pas de trackers de merde, utilisez ça plutôt (https://en.wikipedia.org/wiki/UTM_parameters)"
        --      in pure $ Just response

-- | return a (Just url) if the given raw Url had any tracking shit in the query.
-- The returned url is stripped off the tracking markers
-- https://en.wikipedia.org/wiki/UTM_parameters
parseUrlMessage :: Text -> Maybe Text
parseUrlMessage = hush . M.parse urlParser ""

-- | return an updated URL if some tracker were removed
withoutTracker :: Text -> Maybe Text
withoutTracker rawUrl = case M.parseMaybe parser rawUrl of
  Nothing -> Just rawUrl
  Just uri ->
    let newUri = uri & Fields.field @"uriQuery" %~ (filter (not . isTrackerParam))
     in if newUri /= uri
          then Just (URI.render newUri)
          else Nothing
  where
    parser :: M.Parsec Text Text URI.URI
    parser = URI.parser

    isTrackerParam :: URI.QueryParam -> Bool
    isTrackerParam = \case
      URI.QueryFlag k ->
        let t = URI.unRText k
         in any (== t) trackers
      URI.QueryParam k _ ->
        let t = URI.unRText k
         in any (== t) trackers

    trackers = ["utm_source", "utm_medium", "utm_campaign", "utm_term", "utm_content"]

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
urlParser' = M.between
  (optional (M.oneOf ("([" :: String)))
  (optional (M.oneOf ("])" :: String)))
  $ do
    proto <- C.string' "http://" <|> C.string' "https://"
    rest <- LC.P.urlWord
    pure $ proto <> rest

fetchUrlData ::
  MonadIO m =>
  LC.St.YoutubeAPIKey ->
  Text ->
  m (Either FetchError Text)
fetchUrlData ytApiKey rawUrl =
  if isYoutube rawUrl
    then runFetch $ fetchYtUrlData ytApiKey rawUrl
    else runFetch $ fetchGeneralUrlData rawUrl

data FetchError
  = UnparsableUrl
  | HttpExc Req.HttpException
  | InvalidContentType ByteString
  | DecodingError UnicodeException
  | TitleNotFound
  | YoutubeIdNotFound
  | NoVideoFound Text
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
  NoVideoFound ytId ->
    "No video found for id: " <> ytId <> " (video removed most likely)"

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
      "https://www.youtu.be",
      "https://m.youtube.com"
    ]

fetchGeneralUrlData ::
  ( MonadIO m,
    Req.MonadHttp m,
    Ex.MonadError FetchError m
  ) =>
  Text ->
  m Text
fetchGeneralUrlData textUrl = do
  -- drop any fragment in the url since this shouldn't be transmitted to
  -- the server. See https://github.com/mrkkrp/req/issues/73
  -- which leads to https://github.com/snoyberg/http-client/issues/424
  parsedUrl <- case parseUrl (T.takeWhile (/= '#') textUrl) of
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
  where
    parseUrl raw = do
      uri <- M.parseMaybe parser raw
      Req.useURI uri

    parser :: M.Parsec Text Text URI.URI
    parser = URI.parser

parseTitle :: Text -> Maybe Text
parseTitle body =
  let tags = HTML.parseTags body
      mbTitle = dropWhile (not . isTitleTag) tags
      replaceNewLines c = if c == '\n' then ' ' else c
   in case mbTitle of
        [] -> Nothing
        [_] -> Nothing
        (_ : title : _) -> Just (T.map replaceNewLines $ HTML.fromTagText title)

isTitleTag :: HTML.Tag Text -> Bool
isTitleTag tag = HTML.isTagOpenName "title" tag || HTML.isTagOpenName "TITLE" tag

newtype YtResponseSnippets a = YtResponseSnippets {getYtResponseSnippets :: Vector a}
  deriving (Show)

instance (JSON.FromJSON a) => JSON.FromJSON (YtResponseSnippets a) where
  parseJSON = JSON.withObject "videoOverview" $ \o -> do
    YtResponseSnippets . fmap getSnippet <$> o .: "items"

newtype Snippet a = Snippet {getSnippet :: a}

instance (JSON.FromJSON a) => JSON.FromJSON (Snippet a) where
  parseJSON = JSON.withObject "snippet" $ \o -> Snippet <$> o .: "snippet"

newtype ChannelId = ChannelId {getChannelId :: Text}
  deriving stock (Show)
  deriving newtype (JSON.FromJSON)

data YtVideo = YtVideo
  { yvTitle :: Text,
    yvChannelId :: ChannelId
  }
  deriving (Show)

instance JSON.FromJSON YtVideo where
  parseJSON = JSON.withObject "video" $ \v ->
    YtVideo <$> v .: "title" <*> v .: "channelId"

newtype YtChannel = YtChannel
  { ycTitle :: Text
  }
  deriving stock (Show)

instance JSON.FromJSON YtChannel where
  parseJSON = JSON.withObject "channel" $ \c ->
    YtChannel <$> c .: "title"

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
  (YtResponseSnippets overview) <-
    either Ex.throwError (pure . Req.responseBody)
      =<< runFetch
        ( Req.req
            Req.GET
            (Req.https "www.googleapis.com" /: "youtube" /: "v3" /: "videos")
            Req.NoReqBody
            Req.jsonResponse
            ( "part" =: ("snippet" :: Text)
                <> "id" =: ytId
                <> "key" =: LC.St.getYoutubeAPIKey ytApiKey
            )
        )
  video <- case overview !? 0 of
    Nothing -> Ex.throwError (NoVideoFound ytId)
    Just v -> pure v
  channel <- fetchYtChannel ytApiKey (yvChannelId video)
  pure $ yvTitle video <> " [" <> ycTitle channel <> "]"

fetchYtChannel ::
  ( MonadIO m,
    Req.MonadHttp m,
    Ex.MonadError FetchError m
  ) =>
  LC.St.YoutubeAPIKey ->
  ChannelId ->
  m YtChannel
fetchYtChannel ytApiKey (ChannelId channelId) = do
  (YtResponseSnippets resp) <-
    either Ex.throwError (pure . Req.responseBody)
      =<< runFetch
        ( Req.req
            Req.GET
            (Req.https "www.googleapis.com" /: "youtube" /: "v3" /: "channels")
            Req.NoReqBody
            Req.jsonResponse
            ( "part" =: ("snippet" :: Text)
                <> "id" =: channelId
                <> "key" =: LC.St.getYoutubeAPIKey ytApiKey
            )
        )
  case resp !? 0 of
    Nothing -> throwString $ "Cannot find channel with id " <> T.unpack channelId
    Just chan -> pure chan

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

-- useful test to quickly check the response from an api request
test :: IO ()
test = do
  ytApiKey <- T.pack <$> getEnv "YT_API_KEY"
  stuff <- runFetch $ do
    ( Req.req
        Req.GET
        (Req.https "www.googleapis.com" /: "youtube" /: "v3" /: "videos")
        Req.NoReqBody
        Req.bsResponse
        ( "part" =: ("snippet" :: Text)
            <> "id" =: ("J0JhMt4MtLw" :: Text)
            <> "key" =: ytApiKey
        )
      )
  case stuff of
    Left err -> sayShow err
    Right resp -> say $ decodeUtf8Lenient $ Req.responseBody resp
