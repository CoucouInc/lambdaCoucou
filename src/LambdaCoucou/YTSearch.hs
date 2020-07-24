module LambdaCoucou.YTSearch where

-- https://developers.google.com/youtube/v3/docs/search

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

searchYt ::
  (MonadIO m) =>
  LC.St.YoutubeAPIKey ->
  [Text] ->
  m (Either Url.FetchError Text)
searchYt ytApiKey queryWords = Url.runFetch $ do
  (videos :: Url.YtResponseSnippets Url.YtVideo) <-
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
  case (Url.getYtResponseSnippets videos) !? 0 of
    Nothing -> pure "No video found 😱"
    Just vid -> do
      channel <- Url.fetchYtChannel ytApiKey (Url.yvChannelId vid)
      pure $ unescape $ Url.yvTitle vid <> " [" <> Url.ycTitle channel <> "]"


-- poor's man solution to remove html escape chars. &quot; -> "
unescape :: Text -> Text
unescape = replace "&quot;" "\""
