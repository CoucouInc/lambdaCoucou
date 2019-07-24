{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LambdaCoucou.Run where

import Control.Monad
import System.Random (getStdGen)
import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Concurrent.STM.TBQueue as STM
import Control.Concurrent.Async (withAsync)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import qualified Text.Megaparsec.Error as Error
import Development.GitRev

import qualified Network.IRC.Client as IRC
import qualified Control.Retry as Retry

import qualified LambdaCoucou.Types as T
import qualified LambdaCoucou.Parser as Parser
import qualified LambdaCoucou.Command as Cmd
import LambdaCoucou.Social (updateLastSeen, sendTellMessages, setupReminders)
import LambdaCoucou.Db (readSocial, readFactoids, updateDb)
import LambdaCoucou.Url (updateLastUrl)

run :: T.Opts -> IO ()
run opts = do
    let host   = T.optHost opts
    let port   = T.optPort opts
    let nick   = T.optNick opts
    let chan   = T.optChan opts
    let logger = IRC.stdoutLogger
    conn <- IRC.connectWithTLS' logger host port 1
    let cfg = IRC.defaultIRCConf nick
    let commandHandler =
            IRC.EventHandler "command handler" IRC.EPrivmsg commandHandlerFunc
    let joinHandler = IRC.EventHandler "join handler" IRC.EJoin onJoinFunc
    let
        cfg' = cfg
            { IRC._eventHandlers = commandHandler
                : joinHandler
                : IRC._eventHandlers cfg
            , IRC._channels = [chan]
            , IRC._ctcpVer = "λcoucou from https://github.com/CoucouInc/lambdaCoucou/"
            }
    state <- initialState
    let writerQueue = T._writerQueue state
    withAsync (updateDb writerQueue) $ \_ -> runWithRetry conn cfg' state

runWithRetry
    :: IRC.ConnectionConfig T.BotState
    -> IRC.InstanceConfig T.BotState
    -> T.BotState
    -> IO ()
runWithRetry conn config state =
    Retry.recoverAll (Retry.exponentialBackoff 50000 <> Retry.limitRetries 10)
        $ \retryStatus -> do
              when (Retry.rsIterNumber retryStatus > 0)
                  $  putStrLn
                  $  "Encountered error, retrying with "
                  <> show retryStatus
              IRC.startStateful conn config state

commandHandlerFunc :: IRC.UnicodeEvent -> IRC.StatefulIRC T.BotState ()
commandHandlerFunc ev = do
    let (IRC.Privmsg _ eitherMessage) = IRC._message ev
    case eitherMessage of
        Left _ -> return () -- CTCP
        Right raw -> do
            updateLastSeen (IRC._source ev)
            sendTellMessages ev
            updateLastUrl (IRC._source ev) raw
            case Parser.parseCommand raw of
                Left err ->
                    liftIO . print $ "parse error: " <> Error.parseErrorPretty err
                Right cmd -> do
                    liftIO $ print $ "got command: " <> show cmd
                    Cmd.handleCommand ev cmd


initialState :: IO T.BotState
initialState = do
    eitherFactoids <- readFactoids
    eitherSocial <- readSocial
    case (eitherFactoids, eitherSocial) of
        (Left parseError, _) -> error $ "Error reading factoids: " <> parseError
        (_, Left parseError) -> error $ "Error reading social db: " <> parseError
        (Right factoids, Right socialDb) -> do
            facts <- STM.newTVarIO factoids
            social <- STM.newTVarIO socialDb
            stdGen <- getStdGen >>= STM.newTVarIO
            lastUrl <- STM.newTVarIO Nothing
            writerQueue <- STM.newTBQueueIO 10
            return
                T.BotState
                { T._stdGen = stdGen
                , T._factoids = facts
                , T._socialDb = social
                , T._writerQueue = writerQueue
                , T._version = ($(gitHash), $(gitCommitDate))
                , T._lastUrl = lastUrl
                }

onJoinFunc :: IRC.UnicodeEvent -> IRC.StatefulIRC T.BotState ()
onJoinFunc ev = case IRC._message ev of
        IRC.Join _chan -> setupReminders
        _ -> return ()
