{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Command where

import Control.Monad (unless, forM_)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)
import System.Random (randomR)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Concurrent.STM.TBQueue as Queue
import qualified Network.IRC.Client as IRC
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V

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
        T.SetFactoid val -> setFactoid target name factoids val

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
    queue <- T._writerQueue <$> IRC.state
    factoids <- 
        liftIO $
        STM.atomically $ do
            factoids <- STM.readTVar factoidsT
            let factoids' = Map.adjust (incdecCounter op) name factoids
            STM.writeTVar factoidsT factoids'
            factoids' <- STM.readTVar factoidsT
            if factoids == factoids'
            then return Nothing
            else do
                Queue.writeTBQueue queue factoids'
                return $ Just factoids'
    forM_ factoids (sendFactoid target name)

incdecCounter :: (Int -> Int) -> T.Factoid -> T.Factoid
incdecCounter op (T.Counter n) = T.Counter $! op n
incdecCounter _ f = f

setFactoid :: Text -> Text -> T.Factoids -> Text -> IRC.StatefulIRC T.BotState ()
setFactoid target name factoids val =
    case Map.lookup name factoids of
        Just (T.Counter _) ->
            IRC.send $
            IRC.Privmsg
                target
                (Right "Nope ! (There is already a factoid for that.)")
        Just (T.Facts facts) -> do
            let msg =
                    if length facts > 1
                        then "Nope ! (There are existing factoids for that.)"
                        else "Nope ! (There is already a factoid for that.)"
            IRC.send $ IRC.Privmsg target (Right msg)
        Nothing -> do
            state <- IRC.state
            let factoidsT = T._factoids state
            let mbCounter = readMaybe (unpack val)
            let fact = maybe (T.Facts (V.singleton val)) T.Counter mbCounter
            liftIO $
                STM.atomically $ do
                    factoids <- STM.readTVar factoidsT
                    let factoids' = Map.insert name fact factoids
                    STM.writeTVar factoidsT factoids'
                    Queue.writeTBQueue (T._writerQueue state) factoids'
            IRC.send $ IRC.Privmsg target (Right "C'est noté !")
