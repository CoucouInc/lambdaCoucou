{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Factoids where

import Data.Monoid ((<>))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomR)
import Text.Read (readMaybe)
import Data.Text (Text, pack, unpack, intercalate)
import qualified Data.Vector as V
import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as Map

import qualified Network.IRC.Client as IRC

import qualified LambdaCoucou.Types as T
import LambdaCoucou.Db (updateFactoids)

getFactoid :: Text -> IRC.StatefulIRC T.BotState (Maybe Text)
getFactoid name = do
    factoidsT <- T._factoids <$> IRC.state
    factoids <- liftIO $ STM.readTVarIO factoidsT
    liftIO $ print $ "get factoid " <> name
    case Map.lookup name factoids of
        Nothing -> return Nothing
        Just fact ->
            case fact of
                T.Counter n ->
                    let payload = name <> ": " <> pack (show n)
                    in return (Just payload)
                T.Facts facts -> getRandomFactoid facts

getFactoids :: Text -> IRC.StatefulIRC T.BotState Text
getFactoids name = do
    factoidsT <- T._factoids <$> IRC.state
    factoids <- liftIO $ STM.readTVarIO factoidsT
    liftIO $ print $ "get factoids " <> name
    return $ case Map.lookup name factoids of
        Nothing -> "Pas de factoids pour: " <> name
        Just (T.Counter n) -> "Counter: " <> pack (show n)
        Just (T.Facts facts) -> intercalate " | " (V.toList facts)


getRandomFactoid :: V.Vector Text -> IRC.StatefulIRC T.BotState (Maybe Text)
getRandomFactoid facts =
    if V.null facts
        then return Nothing
        else do
            genT <- T._stdGen <$> IRC.state
            idx <-
                liftIO $
                STM.atomically $
                do gen <- STM.readTVar genT
                   let (a, gen') = randomR (0, V.length facts - 1) gen
                   STM.writeTVar genT gen'
                   return a
            return . Just $ facts V.! idx

adjustCounterFactoid :: (Int -> Int) -> IRC.UnicodeEvent -> Text -> IRC.StatefulIRC T.BotState ()
adjustCounterFactoid op ev name = do
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
    forM_ factoids (const $ IRC.reply ev name) -- sendFactoid target name)

incdecCounter :: (Int -> Int) -> T.Factoid -> T.Factoid
incdecCounter op (T.Counter n) = T.Counter $! op n
incdecCounter _ f = f

setFactoid :: IRC.UnicodeEvent -> Text -> Text -> IRC.StatefulIRC T.BotState ()
setFactoid ev name val = do
    state <- IRC.state
    let factoidsT = T._factoids state
    factoids <- liftIO $ STM.readTVarIO factoidsT
    case Map.lookup name factoids of
        Just (T.Counter _) -> IRC.reply ev "Nope ! (There is already a factoid for that.)"
        Just (T.Facts facts) -> do
            let msg =
                    if length facts > 1
                        then "Nope ! (There are existing factoids for that.)"
                        else "Nope ! (There is already a factoid for that.)"
            IRC.reply ev msg
        Nothing -> do
            let mbCounter = readMaybe (unpack val)
            let fact = maybe (T.Facts (V.singleton val)) T.Counter mbCounter
            liftIO $
                STM.atomically $
                do newFactoids <- STM.readTVar factoidsT
                   let factoids' = Map.insert name fact newFactoids
                   STM.writeTVar factoidsT factoids'
                   updateFactoids (T._writerQueue state) factoids'
            IRC.reply ev "C'est noté !"

deleteFactoid :: IRC.UnicodeEvent -> Text -> IRC.StatefulIRC T.BotState ()
deleteFactoid ev name = do
    factoidsT <- T._factoids <$> IRC.state
    writerQ <- T._writerQueue <$> IRC.state
    liftIO . STM.atomically $ do
        STM.modifyTVar' factoidsT (Map.delete name)
        factoids' <- STM.readTVar factoidsT
        updateFactoids writerQ factoids'
    IRC.reply ev "C'est effacé !"

-- not quite thread safe, but hell with it
resetFactoid :: IRC.UnicodeEvent -> Text -> Text -> IRC.StatefulIRC T.BotState ()
resetFactoid ev name val = do
    factoidsT <- T._factoids <$> IRC.state
    liftIO . STM.atomically $ STM.modifyTVar' factoidsT (Map.delete name)
    setFactoid ev name val

augmentFactoid :: IRC.UnicodeEvent -> Text -> Text -> IRC.StatefulIRC T.BotState ()
augmentFactoid ev name val = do
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
    IRC.reply ev msg

