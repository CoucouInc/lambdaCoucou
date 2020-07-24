{-# LANGUAGE StrictData #-}

module LambdaCoucou.YTSearch where

-- https://developers.google.com/youtube/v3/docs/search

import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import qualified LambdaCoucou.HandlerUtils as LC.Hdl
import qualified LambdaCoucou.State as LC.St
import qualified LambdaCoucou.Url as Url
import Network.HTTP.Req ((/:), (=:))
import qualified Network.HTTP.Req as Req
import qualified Network.IRC.Client as IRC.C
import RIO
import qualified RIO.State as St
import qualified RIO.Text as T
import RIO.Text.Partial (replace)
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

data SearchResult = SearchResult
  { srVideoId :: Text,
    srTitle :: Text,
    srChannelId :: Url.ChannelId
  }
  deriving stock (Show)

instance JSON.FromJSON SearchResult where
  parseJSON = JSON.withObject "SearchResult" $ \o -> do
    i <- o .: "id" >>= JSON.withObject "videoId" (.: "videoId")
    (title, chanId) <-
      o .: "snippet"
        >>= JSON.withObject
          "id"
          (\os -> (,) <$> os .: "title" <*> os .: "channelId")
    pure $
      SearchResult
        { srVideoId = i,
          srTitle = title,
          srChannelId = chanId
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
    Nothing -> pure "No video found 😱"
    Just SearchResult {srVideoId, srTitle, srChannelId} -> do
      channel <- Url.fetchYtChannel ytApiKey srChannelId
      pure $ unescape $ srTitle <> " [" <> Url.ycTitle channel <> "] " <> mkUrl srVideoId
  where
    mkUrl vidId = "https://www.youtube.com/watch?v=" <> vidId

-- poor's man solution to remove html escape chars. &quot; -> "
unescape :: Text -> Text
unescape = replace "&amp;" "&" . replace "&quot;" "\""
