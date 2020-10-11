module LambdaCoucou.Sed (saveMessage, sedCommandHandler) where

import Data.Foldable
import qualified Data.RingBuffer as RB
import qualified LambdaCoucou.State as LC.St
import qualified Network.IRC.Client as IRC.C
import RIO
import qualified RIO.Map as Map
import qualified RIO.State as St
import qualified RIO.Text as T
import Text.Regex.PCRE.Heavy ((=~))
import qualified Text.Regex.PCRE.Heavy as Re

saveMessage :: (MonadIO m, St.MonadState LC.St.CoucouState m) => LC.St.ChannelName -> Text -> m ()
saveMessage chanName message = do
  Map.lookup chanName <$> St.gets LC.St.csChannels >>= \case
    Nothing -> pure ()
    Just chanState -> liftIO $ RB.append message (LC.St.cstLastMessages chanState)

sedCommandHandler :: LC.St.ChannelName -> Text -> Text -> IRC.C.IRC LC.St.CoucouState (Maybe Text)
sedCommandHandler chanName rawRegex replacementText = case Re.compileM (encodeUtf8 rawRegex) [] of
  Left err -> pure $ Just $ T.pack err
  Right regex ->
    Map.lookup chanName <$> St.gets LC.St.csChannels >>= \case
      Nothing -> pure Nothing -- that should not happen, but who knows
      Just chanState -> do
        lastMessages <-
          -- remove messages which look like sed commands
          filter (not . sedCommandLike)
            <$> (liftIO $ RB.toList (LC.St.cstLastMessages chanState))
        -- TODO: run the following line where the regex replacement happen in a separate thread
        -- with a timeout and an allocation limit to avoid denial of service through pathological regex
        -- (and fool around the ghc memory module)
        let result = foldl (\prev msg -> prev <|> replace regex replacementText msg) Nothing lastMessages
        pure result

sedCommandLike :: Text -> Bool
sedCommandLike t = "s/" `T.isPrefixOf` t

replace :: Re.Regex -> Text -> Text -> Maybe Text
replace regex replacementText msg =
  if msg =~ regex
    then Just (Re.gsub regex replacementText msg)
    else Nothing
