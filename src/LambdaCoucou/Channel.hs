{-# LANGUAGE TypeApplications #-}

module LambdaCoucou.Channel where

import Control.Lens (at, (<%=), _Just)
import qualified Control.Monad.State.Strict as St
import Data.Generics.Product.Fields (field)
import qualified Data.RingBuffer as RB
import qualified LambdaCoucou.State as LC.St
import qualified Network.IRC.Client as IRC.C
import qualified Network.IRC.Client.Events as IRC.Ev
import RIO
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Set.Partial as Set'
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T'
import System.IO (putStrLn)
import qualified System.Random as Rng

channelStateHandlers :: [IRC.Ev.EventHandler LC.St.CoucouState]
channelStateHandlers =
  [ onJoinChannelState,
    onChangeHandler,
    onJoinHandler,
    onPartHandler,
    onQuitHandler
  ]

data InvalidNameReply = InvalidNameReply
  deriving (Show, Typeable)

instance Exception InvalidNameReply

data InvalidChannelType = InvalidChannelType
  deriving (Show, Typeable)

instance Exception InvalidChannelType

--  353 == RPL_NAMREPLY list of nicks in a given channel
onJoinChannelState :: IRC.Ev.EventHandler LC.St.CoucouState
onJoinChannelState =
  IRC.Ev.EventHandler
    (IRC.Ev.matchNumeric 353)
    ( \_source args -> do
        nick <- getOwnNick
        case args of
          [_ownNick, rawChanType, chanName, nickList] ->
            case parseChanType rawChanType of
              Nothing -> throwM InvalidChannelType
              Just chanType -> do
                lastUrls <- liftIO $ RB.new 10
                lastMessages <- liftIO $ RB.new 10
                let nicks =
                      fmap parseNick $
                        filter (/= nick) $
                          T.words nickList
                let chanSt =
                      LC.St.ChannelState
                        { LC.St.cstUsers = Set.fromList nicks,
                          LC.St.cstType = chanType,
                          LC.St.cstLastUrls = lastUrls,
                          LC.St.cstCoucouCount = 0,
                          LC.St.cstLastMessages = lastMessages
                        }
                field @"csChannels" <%= Map.insert (LC.St.ChannelName chanName) chanSt
          _ -> throwM InvalidNameReply
        pure ()
    )

parseNick :: Text -> Text
parseNick n
  | T.null n = n
  | T'.head n == '*' || T'.head n == '@' = T'.tail n
  | otherwise = n

parseChanType :: Text -> Maybe LC.St.ChannelType
parseChanType ct = case T.uncons ct of
  Just (typ, rest) | T.null rest -> case typ of
    '@' -> Just LC.St.Secret
    '*' -> Just LC.St.Private
    '=' -> Just LC.St.Public
    _ -> Nothing
  _ -> Nothing

onJoinHandler :: IRC.Ev.EventHandler LC.St.CoucouState
onJoinHandler =
  IRC.Ev.EventHandler
    (IRC.Ev.matchType IRC.Ev._Join)
    ( \source _args -> case source of
        (IRC.Ev.Channel chanName nick) -> addNickToChan chanName nick
        _ -> pure ()
        -- this should never happen for a JOIN message though
    )

onPartHandler :: IRC.Ev.EventHandler LC.St.CoucouState
onPartHandler =
  IRC.Ev.EventHandler
    ( \ev ->
        void (IRC.Ev.matchType IRC.Ev._Part ev)
          <|> void (IRC.Ev.matchType IRC.Ev._Kick ev)
    )
    ( \source _ -> case source of
        (IRC.Ev.Channel chanName nick) -> removeNickFromChan chanName nick
        x -> liftIO $ putStrLn $ "WARN! Malformed PART or KICK message, got: " <> show x
    )

onChangeHandler :: IRC.Ev.EventHandler LC.St.CoucouState
onChangeHandler =
  IRC.Ev.EventHandler
    (IRC.Ev.matchType IRC.Ev._Nick)
    ( \source newNick -> case source of
        (IRC.Ev.User oldNick) -> do
          nick <- getOwnNick
          when (nick /= newNick) $
            void $
              field @"csChannels"
                <%= fmap
                  ( \chanState ->
                      chanState
                        & field @"cstUsers" %~ (Set.insert newNick . Set.delete oldNick)
                  )
        x -> liftIO $ putStrLn $ "WARN! Malformed Nick message, got: " <> show x
    )

onQuitHandler :: IRC.Ev.EventHandler LC.St.CoucouState
onQuitHandler =
  IRC.Ev.EventHandler
    (IRC.Ev.matchType IRC.Ev._Quit)
    ( \source _reason -> case source of
        (IRC.Ev.User nick) -> do
          liftIO $ putStrLn $ "+++++++ removing user from state: " <> show nick
          field @"csChannels"
            <%= fmap
              ( \chanState ->
                  chanState
                    & field @"cstUsers" %~ Set.delete nick
              )
          pure ()
        x -> liftIO $ putStrLn $ "++++++++++ WARN! Malformed Quit message, got: " <> show x
    )

addNickToChan :: (St.MonadState LC.St.CoucouState m) => Text -> Text -> m ()
addNickToChan chanName nick =
  void $
    field @"csChannels" . at (LC.St.ChannelName chanName) . _Just . field @"cstUsers"
      <%= Set.insert nick

removeNickFromChan :: (St.MonadState LC.St.CoucouState m) => Text -> Text -> m ()
removeNickFromChan chanName nick =
  void $
    field @"csChannels" . at (LC.St.ChannelName chanName) . _Just . field @"cstUsers"
      <%= Set.delete nick

getOwnNick :: IRC.C.IRC s Text
getOwnNick = do
  instanceCfg <- IRC.C.getIRCState >>= IRC.C.snapshot IRC.C.instanceConfig
  pure $ instanceCfg ^. IRC.C.nick

shoutCoucouCommandHandler :: LC.St.ChannelName -> IRC.C.IRC LC.St.CoucouState (Maybe Text)
shoutCoucouCommandHandler chanName = do
  usrList <- St.gets (^. field @"csChannels" . at chanName . _Just . field @"cstUsers")
  let s = Set.size usrList
  if s < 1
    then pure Nothing
    else do
      idx <- liftIO $ Rng.randomRIO (0, s - 1)
      let usr = Set'.elemAt idx usrList
      pure $ Just $ "coucou " <> usr
