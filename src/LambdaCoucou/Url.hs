module LambdaCoucou.Url where

import Data.Text (Text)

import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM as STM
import qualified Network.IRC.Client as IRC
import qualified LambdaCoucou.Types as T
import qualified LambdaCoucou.Parser as Parser

updateLastUrl :: Text -> IRC.StatefulIRC T.BotState ()
updateLastUrl raw =
    case Parser.parseUrl raw of
        Nothing -> return ()
        Just url -> do
            lastUrlT <- T._lastUrl <$> IRC.state
            liftIO $ STM.atomically $ STM.modifyTVar' lastUrlT (const (Just url))
            return ()
