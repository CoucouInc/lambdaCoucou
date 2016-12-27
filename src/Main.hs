{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random (getStdGen)
import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Concurrent.STM.TBQueue as STM
import Control.Concurrent.Async (withAsync)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Text.Megaparsec.Error as Error

import qualified Network.IRC.Client as IRC

import qualified LambdaCoucou.Types as T
import qualified LambdaCoucou.Parser as Parser
import qualified LambdaCoucou.Command as Cmd
import LambdaCoucou.Factoids (readFactoids, writeFactoids)

main :: IO ()
main = do
    let host = "chat.freenode.net"
    let port = 6697
    let nick = "lambdacoucou"
    run host port nick

run :: ByteString -> Int -> Text -> IO ()
run host port nick = do
    eitherFactoids <- readFactoids
    case eitherFactoids of
        Left parseError -> error $ "Error reading factoids: " <> parseError
        Right factoids -> do
            facts <- STM.newTVarIO factoids
            let logger = IRC.stdoutLogger
            conn <- IRC.connectWithTLS' logger host port 1
            let cfg = IRC.defaultIRCConf nick
            let commandHandler =
                    IRC.EventHandler "command handler" IRC.EPrivmsg commandHandlerFunc
            let cfg' =
                    cfg
                    { IRC._eventHandlers = commandHandler : IRC._eventHandlers cfg
                    , IRC._channels = ["#gougoutest"]
                    }
            stdGen <- getStdGen >>= STM.newTVarIO
            writerQueue <- STM.newTBQueueIO 10
            let initialState = T.BotState stdGen facts writerQueue
            withAsync (writeFactoids writerQueue) $
                \_ -> IRC.startStateful conn cfg' initialState


commandHandlerFunc :: IRC.UnicodeEvent -> IRC.StatefulIRC T.BotState ()
commandHandlerFunc ev = do
    let (IRC.Privmsg target eitherMessage) = IRC._message ev
    case eitherMessage of
        Left _ -> return () -- CTCP
        Right raw ->
            case Parser.parseCommand raw of
                Left err ->
                    liftIO . print $ "parse error: " <> Error.parseErrorPretty err
                Right cmd -> do
                    liftIO $ print $ "got command: " <> show cmd
                    Cmd.handleCommand target cmd
    return ()
