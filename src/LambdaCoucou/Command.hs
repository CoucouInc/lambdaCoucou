{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Command where

import Control.Monad (unless)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import System.Random (randomR)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as STM
import qualified Network.IRC.Client as IRC
import qualified Network.HTTP.Req
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V
import qualified Lens.Micro as L

import qualified LambdaCoucou.Types as T
import LambdaCoucou.Cancer (fetchCancer)

handleCommand :: Text -> T.CoucouCmd -> IRC.StatefulIRC T.BotState ()
handleCommand _ T.CoucouCmdNop = return ()
handleCommand target (T.CoucouCmdCancer search) = do
    liftIO . print $ "cancer maybe with search: " <> show search
    mbcancer <- fetchCancer search
    liftIO . print $ "got cancer: " <> show mbcancer
    case mbcancer of
        Nothing -> return ()
        Just (desc, url) -> do
            let payload = desc <> ": " <> url
            let response = IRC.Privmsg target (Right payload)
            IRC.send response
handleCommand target (T.CoucouCmdFactoid name factoidType) = do
    liftIO . print $ "factoid command"
    factoidsT <- T._factoids <$> IRC.state
    factoids <- liftIO $ STM.readTVarIO factoidsT
    case factoidType of
        T.GetFactoid -> sendFactoid target name factoids
        T.IncFactoid -> adjustCounterFactoid succ target name
        T.DecFactoid -> adjustCounterFactoid pred target name

sendFactoid :: Text -> Text -> T.Factoids -> IRC.StatefulIRC T.BotState ()
sendFactoid target name factoids =
    case Map.lookup name factoids of
        Nothing -> return ()
        Just fact ->
            case fact of
                T.Counter n ->
                    let payload = name <> ": " <> pack (show n)
                    in IRC.send (IRC.Privmsg target (Right payload))
                T.Facts facts -> sendRandomFactoid target facts

sendRandomFactoid :: Text -> V.Vector Text -> IRC.StatefulIRC T.BotState ()
sendRandomFactoid target facts =
    unless (V.null facts) $
    do genT <- T._stdGen <$> IRC.state
       idx <- 
           liftIO $
           STM.atomically $
           do gen <- STM.readTVar genT
              let (a, gen') = randomR (0, V.length facts - 1) gen
              STM.writeTVar genT gen'
              return a
       let factoid = facts V.! idx
       let msg = IRC.Privmsg target (Right factoid)
       IRC.send msg

adjustCounterFactoid :: (Int -> Int)
                     -> Text
                     -> Text
                     -> IRC.StatefulIRC T.BotState ()
adjustCounterFactoid op target name = do
    factoidsT <- T._factoids <$> IRC.state
    factoids <- 
        liftIO $
        STM.atomically $
        do STM.modifyTVar' factoidsT (Map.adjust (incdecCounter op) name)
           STM.readTVar factoidsT
    sendFactoid target name factoids

incdecCounter :: (Int -> Int) -> T.Factoid -> T.Factoid
incdecCounter op (T.Counter n) = T.Counter $ op n
incdecCounter _ f = f
