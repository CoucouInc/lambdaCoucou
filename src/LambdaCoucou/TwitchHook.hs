{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module LambdaCoucou.TwitchHook where

import RIO
import qualified RIO.Time as Time

import System.IO (putStr)
import qualified Control.Monad.STM as STM
import Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM.TBMChan as Chan
import qualified Network.Wai.Handler.Warp as Warp
import Servant.API ((:>))
import qualified Servant.API as SAPI
import Servant.API.Generic ((:-))
import qualified Servant.API.Generic as GSAPI
import qualified Servant.Server as SS
import qualified Servant.Server.Generic as GSS
import qualified Network.Wai.Middleware.RequestLogger as Wai.Logger

import LambdaCoucou.TwitchTypes

data WebhookRoutes route
  = WebhookRoutes
      -- TODO: add the hub.secret thingy
      { callback ::
          route
            :- "twitch"
            :> "notifications"
            :> SAPI.QueryParam' '[SAPI.Required] "hub.mode" HubMode
            :> SAPI.QueryParam' '[SAPI.Required] "hub.topic" HubTopic
            :> SAPI.QueryParam' '[SAPI.Required] "hub.challenge" HubChallenge
            :> SAPI.QueryParam' '[SAPI.Optional] "hub.lease_seconds" HubLeaseSeconds
            :> SAPI.Get '[SAPI.PlainText] HubChallenge,
        notification ::
          route
            :- "twitch"
            :> "notifications"
            :> SAPI.ReqBody '[SAPI.JSON] StreamNotification
            :> SAPI.Post '[SAPI.PlainText] SAPI.NoContent
      }
  deriving (Generic)


data WebhookServerState
  = WebhookServerState
      { leases :: [Lease],
        notifChan :: Chan.TBMChan StreamNotification
      }
  deriving (Generic)

webhookAPI :: Proxy (GSAPI.ToServantApi WebhookRoutes)
webhookAPI = GSAPI.genericApi (Proxy @WebhookRoutes)

callbackHandler ::
  MVar.MVar [Lease] ->
  HubMode ->
  HubTopic ->
  HubChallenge ->
  Maybe HubLeaseSeconds ->
  SS.Handler HubChallenge
callbackHandler leases hubMode topic challenge mbLease =
  case hubMode of
    Unsubscribe -> do
      logStr $ "unsubscribing from topic: " <> show topic
      liftIO $ MVar.modifyMVar_ leases $ \ls -> pure (filter (\l -> leaseTopic l /= topic) ls)
      pure challenge
    Subscribe -> case mbLease of
      Nothing -> throwIO $ SS.err400 {SS.errBody = "Missing required parameter: lease_seconds"}
      Just (HubLeaseSeconds leaseSeconds) -> do
        logStr $ "subscribing to topic: " <> show topic
        now <- liftIO Time.getCurrentTime
        let expiresAt = Time.addUTCTime (fromIntegral leaseSeconds) now
        let newLease = Lease {leaseTopic = topic, leaseExpiresAt = expiresAt}
        liftIO $ MVar.modifyMVar_ leases $ \ls ->
          -- remove any existing lease with the same topic. This will happen
          -- when renewing a lease
          pure $ newLease : filter (\l -> leaseTopic l /= topic) ls
        pure challenge

notificationHandler ::
  Chan.TBMChan StreamNotification ->
  StreamNotification ->
  SS.Handler SAPI.NoContent
notificationHandler chan notification = do
  liftIO $ STM.atomically $ Chan.writeTBMChan chan notification
  pure SAPI.NoContent

webhookServerRecord :: ClientEnv -> WebhookRoutes GSS.AsServer
webhookServerRecord env = WebhookRoutes
  { callback = callbackHandler (ceNotificationLeases env),
    notification = notificationHandler (ceNotifChan env)
  }

runWebhookServer :: ClientEnv -> IO ()
runWebhookServer env = do
  let app = GSS.genericServe $ webhookServerRecord env
  Warp.run (ceWebhookServerPort env) (Wai.Logger.logStdout app)

iso8601 :: Time.UTCTime -> String
iso8601 = Time.formatTime Time.defaultTimeLocale "%FT%T%QZ"

logStr :: (MonadIO m) => String -> m ()
logStr msg = liftIO $ do
  now <- Time.getCurrentTime
  putStr $ iso8601 now <> " " <> msg <> "\n"
