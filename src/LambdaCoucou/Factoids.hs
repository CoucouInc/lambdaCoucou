{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Factoids where

import Data.Monoid ((<>))
import Control.Monad (unless, forM_)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomR)
import Text.Read (readMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Vector as V
import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as Map

import qualified Network.IRC.Client as IRC

import qualified LambdaCoucou.Types as T
import LambdaCoucou.Connection (sendPrivMsg)
import LambdaCoucou.Db (updateFactoids)

sendFactoid :: Text -> Text -> IRC.StatefulIRC T.BotState ()
sendFactoid target name = do
    factoidsT <- T._factoids <$> IRC.state
    factoids <- liftIO $ STM.readTVarIO factoidsT
    case Map.lookup name factoids of
        Nothing -> return ()
        Just fact ->
            case fact of
                T.Counter n ->
                    let payload = name <> ": " <> pack (show n)
                    in sendPrivMsg target payload
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
       sendPrivMsg target factoid

adjustCounterFactoid :: (Int -> Int) -> Text -> Text -> IRC.StatefulIRC T.BotState ()
adjustCounterFactoid op target name = do
    factoidsT <- T._factoids <$> IRC.state
    queue <- T._writerQueue <$> IRC.state
    factoids <-
        liftIO $
        STM.atomically $
        do factoids <- STM.readTVar factoidsT
           let factoids' = Map.adjust (incdecCounter op) name factoids
           STM.writeTVar factoidsT factoids'
           if factoids == factoids'
               then return Nothing
               else do
                   updateFactoids queue factoids'
                   return $ Just factoids'
    forM_ factoids (const $ sendFactoid target name)

incdecCounter :: (Int -> Int) -> T.Factoid -> T.Factoid
incdecCounter op (T.Counter n) = T.Counter $! op n
incdecCounter _ f = f

setFactoid :: Text -> Text -> Text -> IRC.StatefulIRC T.BotState ()
setFactoid target name val = do
    state <- IRC.state
    let factoidsT = T._factoids state
    factoids <- liftIO $ STM.readTVarIO factoidsT
    case Map.lookup name factoids of
        Just (T.Counter _) -> sendPrivMsg target "Nope ! (There is already a factoid for that.)"
        Just (T.Facts facts) -> do
            let msg =
                    if length facts > 1
                        then "Nope ! (There are existing factoids for that.)"
                        else "Nope ! (There is already a factoid for that.)"
            sendPrivMsg target msg
        Nothing -> do
            let mbCounter = readMaybe (unpack val)
            let fact = maybe (T.Facts (V.singleton val)) T.Counter mbCounter
            liftIO $
                STM.atomically $
                do newFactoids <- STM.readTVar factoidsT
                   let factoids' = Map.insert name fact newFactoids
                   STM.writeTVar factoidsT factoids'
                   updateFactoids (T._writerQueue state) factoids'
            sendPrivMsg target "C'est noté !"

deleteFactoid :: Text -> Text -> IRC.StatefulIRC T.BotState ()
deleteFactoid target name = do
    factoidsT <- T._factoids <$> IRC.state
    writerQ <- T._writerQueue <$> IRC.state
    liftIO . STM.atomically $ do
        STM.modifyTVar' factoidsT (Map.delete name)
        factoids' <- STM.readTVar factoidsT
        updateFactoids writerQ factoids'
    sendPrivMsg target "C'est effacé !"

-- not quite thread safe, but hell with it
resetFactoid :: Text -> Text -> Text -> IRC.StatefulIRC T.BotState ()
resetFactoid target name val = do
    factoidsT <- T._factoids <$> IRC.state
    liftIO . STM.atomically $ STM.modifyTVar' factoidsT (Map.delete name)
    setFactoid target name val

augmentFactoid :: Text -> Text -> Text -> IRC.StatefulIRC T.BotState ()
augmentFactoid target name val = do
    factoidsT <- T._factoids <$> IRC.state
    writerQ <- T._writerQueue <$> IRC.state
    msg <-
        liftIO . STM.atomically $
        do factoids <- STM.readTVar factoidsT
           case Map.lookup name factoids of
               Nothing -> return $ "No factoid there yet, use `" <> name <> " = " <> val <> "`"
               Just fact -> do
                   let factList =
                           case fact of
                               T.Facts fs -> fs
                               T.Counter n -> V.singleton (pack $ show n)
                   let fact' = T.Facts $ V.snoc factList val
                   let factoids' = Map.insert name fact' factoids
                   STM.writeTVar factoidsT factoids'
                   updateFactoids writerQ factoids'
                   return "C'est noté"
    sendPrivMsg target msg

