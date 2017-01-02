module LambdaCoucou.Connection where

import Data.Text
import qualified Network.IRC.Client as IRC
import qualified LambdaCoucou.Types as T

sendPrivMsg :: Text -> Text -> IRC.StatefulIRC T.BotState ()
sendPrivMsg target msg = IRC.send $ IRC.Privmsg target (Right msg)
