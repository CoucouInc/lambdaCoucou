module LambdaCoucou.Debug where

import RIO
import qualified RIO.State as St
import qualified RIO.Text as T

import qualified LambdaCoucou.State         as LC.St
import qualified Network.IRC.Client         as IRC.C
import qualified Network.IRC.Client.Events  as IRC.Ev

debugEventHandler :: IRC.Ev.EventHandler LC.St.CoucouState
debugEventHandler = IRC.Ev.EventHandler
  (IRC.Ev.matchType IRC.Ev._Privmsg)
  (\source (_target, raw) -> case (source, raw) of
    (IRC.Ev.User nick, Right msg) -> when (nick == "Geekingfrog") $
      when (msg == "state") $ do
        st <- St.get
        IRC.C.replyTo source (T.pack $ show st)

    _ -> pure ()
  )
