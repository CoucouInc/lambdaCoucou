{-# LANGUAGE BangPatterns #-}

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
        result <- liftIO $ findAndReplace regex replacementText lastMessages
        traverse (saveMessage chanName) result
        pure $ fmap ellipsis result

ellipsis :: Text -> Text
ellipsis txt = if T.length txt > 410 then T.take 409 txt <> "…" else txt

sedCommandLike :: Text -> Bool
sedCommandLike t = "s/" `T.isPrefixOf` t

-- wrap the replace operation into a thread with timeout. In some cases
-- the regex can eat all the cpu or behave similarly badly and will
-- make the bot unresponsive (and eventually crash)
-- https://github.com/CoucouInc/lambdaCoucou/issues/26
findAndReplace :: (Traversable t) => Re.Regex -> Text -> t Text -> IO (Maybe Text)
findAndReplace regex replacementText lastMessages = do
  r <-
    timeout
      (10 ^ 4) -- in microseconds
      ( do
          let !r = foldl' (\prev msg -> prev <|> replace regex replacementText msg) Nothing lastMessages
          pure r
      )
  case r of
    Nothing ->
      pure $
        Just
          "Timeout! Regex pourrie, ou moteur de regex pas top. Dans tout les cas samarchpa™ :("
    Just x -> pure x
  where
    wtf prev msg =
      let !replaced = replace regex replacementText msg
       in prev <|> replaced

replace :: Re.Regex -> Text -> Text -> Maybe Text
replace regex replacementText msg =
  if msg =~ regex
    then
      let !r = Re.gsub regex replacementText msg
       in Just r
    else Nothing
