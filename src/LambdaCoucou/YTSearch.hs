{-# LANGUAGE StrictData #-}

module LambdaCoucou.YTSearch where

-- https://developers.google.com/youtube/v3/docs/search

import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import Data.Either as Either
import qualified HTMLEntities.Decoder as HTML
import qualified LambdaCoucou.HandlerUtils as LC.Hdl
import qualified LambdaCoucou.State as LC.St
import qualified LambdaCoucou.Url as Url
import Network.HTTP.Req ((/:), (=:))
import qualified Network.HTTP.Req as Req
import qualified Network.IRC.Client as IRC.C
import RIO
import qualified RIO.State as St
import qualified RIO.Text as T
import RIO.Vector ((!?))

ytSearchCommandHandler ::
  [Text] ->
  Maybe Text ->
  IRC.C.IRC LC.St.CoucouState (Maybe Text)
ytSearchCommandHandler queryWords mbTarget = do
  key <- St.gets LC.St.csYtAPIKey
  result <- searchYt key queryWords
  pure $
    Just $
      LC.Hdl.addTarget mbTarget $
        either
          (Url.showFetchError (T.intercalate "+" queryWords))
          id
          result

newtype SearchResults = SearchResults {getSearchResults :: Vector SearchResult}
  deriving stock (Show)

instance JSON.FromJSON SearchResults where
  parseJSON = JSON.withObject "searchResults" $ \o -> SearchResults <$> o .: "items"

data SearchResult
  = Video VideoSearchResult
  | Channel ChannelSearchResult
  deriving stock (Show)

instance JSON.FromJSON SearchResult where
  parseJSON = JSON.withObject "SearchResult" $ \o -> do
    idKind <- o .: "id" >>= JSON.withObject "id" (.: "kind")
    case idKind of
      JSON.String "youtube#video" -> Video <$> JSON.parseJSON (JSON.Object o)
      JSON.String "youtube#channel" -> Channel <$> JSON.parseJSON (JSON.Object o)
      _ -> fail $ "Unknown id kind for search result: " <> show idKind

data VideoSearchResult = VideoSearchResult
  { vsrVideoId :: Text,
    vsrTitle :: Text,
    vsrChannelTitle :: Text
  }
  deriving stock (Show)

instance JSON.FromJSON VideoSearchResult where
  parseJSON = JSON.withObject "VideoSearchResult" $ \o -> do
    i <- o .: "id" >>= JSON.withObject "id" (.: "videoId")
    (title, chanTitle) <-
      o .: "snippet"
        >>= JSON.withObject
          "snippet"
          (\os -> (,) <$> os .: "title" <*> os .: "channelTitle")
    pure $
      VideoSearchResult
        { vsrVideoId = i,
          vsrTitle = title,
          vsrChannelTitle = chanTitle
        }

data ChannelSearchResult = ChannelSearchResult
  { csrChannelId :: Text,
    csrChannelTitle :: Text
  }
  deriving stock (Show)

instance JSON.FromJSON ChannelSearchResult where
  parseJSON = JSON.withObject "ChannelSearchResult" $ \o -> do
    i <- o .: "id" >>= JSON.withObject "id" (.: "channelId")
    channelId <-
      o .: "snippet"
        >>= JSON.withObject
          "snippet"
          (.: "title")
    pure $
      ChannelSearchResult
        { csrChannelId = i,
          csrChannelTitle = channelId
        }

searchYt ::
  (MonadIO m) =>
  LC.St.YoutubeAPIKey ->
  [Text] ->
  m (Either Url.FetchError Text)
searchYt ytApiKey queryWords = Url.runFetch $ do
  (SearchResults videos) <-
    Req.responseBody
      <$> ( Req.req
              Req.GET
              (Req.https "www.googleapis.com" /: "youtube" /: "v3" /: "search")
              Req.NoReqBody
              Req.jsonResponse
              ( "key" =: LC.St.getYoutubeAPIKey ytApiKey
                  <> "q" =: T.intercalate "+" queryWords
                  <> "part" =: ("snippet" :: Text)
              )
          )

  case videos !? 0 of
    Nothing -> pure $ "😱 Nothing found for query " <> T.unwords queryWords
    Just (Video (VideoSearchResult {vsrVideoId, vsrTitle, vsrChannelTitle})) ->
      pure $ unescape $ vsrTitle <> " [" <> vsrChannelTitle <> "] " <> mkUrl vsrVideoId
    Just (Channel (ChannelSearchResult {csrChannelId, csrChannelTitle})) ->
      pure $ unescape $ csrChannelTitle <> mkChanUrl csrChannelId
  where
    mkUrl vidId = "https://www.youtube.com/watch?v=" <> vidId
    mkChanUrl chanId = "https://www.youtube.com/channel/" <> chanId

unescape :: Text -> Text
unescape t = Either.fromRight t $ HTML.htmlEntityBody t
