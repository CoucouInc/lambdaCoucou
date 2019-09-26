{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module LambdaCoucou.Channel where

import           Control.Applicative
import           Control.Lens                 (at, (%~), (&), (<%=), (^.),
                                               _Just)
import           Control.Monad
import qualified Control.Monad.Catch          as MT
import           Control.Monad.IO.Class       (liftIO)
import qualified Control.Monad.State.Strict   as St
import           Data.Generics.Product.Fields (field)
import qualified Data.Map.Strict              as Map
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as Tx
import qualified Network.IRC.Client           as IRC.C
import qualified Network.IRC.Client.Events    as IRC.Ev
import qualified System.Random                as Rng

import qualified LambdaCoucou.State           as LC.St

channelStateHandlers :: [IRC.Ev.EventHandler LC.St.CoucouState]
channelStateHandlers =
  [ onJoinChannelState
  , onChangeHandler
  , onJoinHandler
  , onPartHandler
  , onQuitHandler
  ]

data InvalidNameReply = InvalidNameReply
  deriving (Show, MT.Exception)

data InvalidChannelType = InvalidChannelType
  deriving (Show, MT.Exception)

--  353 == RPL_NAMREPLY list of nicks in a given channel
onJoinChannelState :: IRC.Ev.EventHandler LC.St.CoucouState
onJoinChannelState = IRC.Ev.EventHandler
  (IRC.Ev.matchNumeric 353)
  (\_source args -> do
    nick <- getOwnNick
    case args of
      [_ownNick, rawChanType, chanName, nickList] ->
        case parseChanType rawChanType of
          Nothing -> MT.throwM InvalidChannelType
          Just chanType -> do
            let nicks = fmap parseNick
                  $ filter (/= nick)
                  $ Tx.words nickList
            let chanSt = LC.St.ChannelState
                  { LC.St.cstUsers = Set.fromList nicks
                  , LC.St.cstType = chanType
                  }
            field @"csChannels" <%= Map.insert (LC.St.ChannelName chanName) chanSt
      _ -> MT.throwM InvalidNameReply
    pure ()
  )

parseNick :: Text -> Text
parseNick n
  | Tx.null n = n
  | Tx.head n == '*' || Tx.head n == '@' = Tx.tail n
  | otherwise = n

parseChanType :: Text -> Maybe LC.St.ChannelType
parseChanType ct = case Tx.uncons ct of
  Just (typ, rest) | Tx.null rest -> case typ of
    '@' -> Just LC.St.Secret
    '*' -> Just LC.St.Private
    '=' -> Just LC.St.Public
    _   -> Nothing
  _ -> Nothing



onJoinHandler :: IRC.Ev.EventHandler LC.St.CoucouState
onJoinHandler = IRC.Ev.EventHandler
  (IRC.Ev.matchType IRC.Ev._Join)
  (\source _args -> case source of
    (IRC.Ev.Channel chanName nick) -> addNickToChan chanName nick
    _ -> pure ()
    -- ^ this should never happen for a JOIN message though
  )

onPartHandler :: IRC.Ev.EventHandler LC.St.CoucouState
onPartHandler = IRC.Ev.EventHandler
  (\ev -> void (IRC.Ev.matchType IRC.Ev._Part ev)
      <|> void (IRC.Ev.matchType IRC.Ev._Kick ev)
  )
  (\source _ -> case source of
    (IRC.Ev.Channel chanName nick) -> removeNickFromChan chanName nick
    x -> liftIO $ putStrLn $ "WARN! Malformed PART or KICK message, got: " <> show x
  )

onChangeHandler :: IRC.Ev.EventHandler LC.St.CoucouState
onChangeHandler = IRC.Ev.EventHandler
  (IRC.Ev.matchType IRC.Ev._Nick)
  (\source newNick -> case source of
    (IRC.Ev.User oldNick) -> do
      nick <- getOwnNick
      when (nick /= newNick) $ void $
        field @"csChannels" <%= fmap (\chanState -> chanState
          & field @"cstUsers" %~ (Set.insert newNick . Set.delete oldNick))
    x -> liftIO $ putStrLn $ "WARN! Malformed Nick message, got: " <> show x
  )

onQuitHandler :: IRC.Ev.EventHandler LC.St.CoucouState
onQuitHandler = IRC.Ev.EventHandler
  (IRC.Ev.matchType IRC.Ev._Quit)
  (\source _reason -> case source of
    (IRC.Ev.User nick) -> do
      liftIO $ putStrLn $ "+++++++ removing user from state: " <> show nick
      field @"csChannels" <%= fmap (\chanState -> chanState
        & field @"cstUsers" %~ Set.delete nick)
      pure ()
    x -> liftIO $ putStrLn $ "++++++++++ WARN! Malformed Quit message, got: " <> show x
  )

addNickToChan :: (St.MonadState LC.St.CoucouState m) => Text -> Text -> m ()
addNickToChan chanName nick = void $
  field @"csChannels" . at (LC.St.ChannelName chanName) . _Just . field @"cstUsers"
    <%= Set.insert nick

removeNickFromChan :: (St.MonadState LC.St.CoucouState m) => Text -> Text -> m ()
removeNickFromChan chanName nick = void $
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
      let usr = Set.elemAt idx usrList
      pure $ Just $ "coucou " <> usr
