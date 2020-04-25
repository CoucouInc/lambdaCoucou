module LambdaCoucou.Debug where

import qualified Control.Concurrent.MVar as MVar
import qualified LambdaCoucou.State as LC.St
import qualified LambdaCoucou.TwitchTypes as Twitch
import qualified Network.IRC.Client as IRC.C
import qualified Network.IRC.Client.Events as IRC.Ev
import RIO
import qualified RIO.State as St
import qualified RIO.Text as T

debugEventHandler :: IRC.Ev.EventHandler LC.St.CoucouState
debugEventHandler =
  IRC.Ev.EventHandler
    (IRC.Ev.matchType IRC.Ev._Privmsg)
    ( \source (_target, raw) -> case (source, raw) of
        (IRC.Ev.User nick, Right msg) -> when (nick == "Geekingfrog")
          $ when (msg == "twitchState")
          $ do
            st <- St.get
            liftIO (MVar.readMVar (LC.St.csTwitch st)) >>= \case
              Nothing -> IRC.C.replyTo source "Twitch module not enabled"
              Just ts -> do
                leases <- liftIO $ MVar.readMVar $ Twitch.ceNotificationLeases ts
                IRC.C.replyTo source (T.pack $ show leases)
        _ -> pure ()
    )
