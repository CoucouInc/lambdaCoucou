{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Command where

import Control.Monad (unless)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import System.Random (randomR)
import qualified GHC.Conc.Sync as Conc
import qualified Network.IRC.Client as IRC
import qualified Network.HTTP.Req
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V

import qualified LambdaCoucou.Types as T
import LambdaCoucou.Cancer (fetchCancer)
import LambdaCoucou.Factoids (readFactoids)


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
    eitherFactoids <- liftIO readFactoids
    case eitherFactoids of
        Left parseError -> liftIO . print $ "Parse error: " <> parseError
        Right factoids -> case factoidType of
            T.GetFactoid -> sendFactoid target name factoids
            -- T.SetFactoid val -> 

sendFactoid :: Text -> Text -> T.Factoids -> IRC.StatefulIRC T.BotState ()
sendFactoid target name factoids = do
    let mbFact = Map.lookup name factoids
    case mbFact of
        Nothing -> return ()
        Just fact ->
            case fact of
                T.Counter n -> undefined
                T.Facts facts -> sendRandomFactoid target facts

sendRandomFactoid :: Text -> V.Vector Text -> IRC.StatefulIRC T.BotState ()
sendRandomFactoid target facts =
    unless (V.null facts) $
    do genT <- T._stdGen <$> IRC.state
       idx <- 
           liftIO $
           Conc.atomically $
           do gen <- Conc.readTVar genT
              let (a, gen') = randomR (0, V.length facts - 1) gen
              Conc.writeTVar genT gen'
              return a
       let factoid = facts V.! idx
       let msg = IRC.Privmsg target (Right factoid)
       IRC.send msg

    -- = GetFactoid
    -- | SetFactoid Text
    -- | ResetFactoid Text
    -- | AugmentFactoid Text
    -- | DeleteFactoid
