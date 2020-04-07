{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module LambdaCoucou.TwitchHook where

import qualified Control.Monad.STM as STM
import qualified Data.ByteString.Lazy as LBS
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM.TBMChan as Chan
import Control.Lens ((%~), (&))
import Control.Monad.IO.Class
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as JSON
import Data.Functor
import Data.Proxy
import qualified Data.Text as Tx
import qualified Data.Time.Clock as Clk
import qualified Data.Vector as V
import GHC.Generics
import qualified LambdaCoucou.Http as LC.HTTP
import Network.HTTP.Req ((/:), (=:))
import qualified Network.HTTP.Req as Req
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant as S
import Servant.API ((:>))
import qualified Servant.API as SAPI
import Servant.API.Generic ((:-))
import qualified Servant.API.Generic as GSAPI
import qualified Servant.Links as SLinks
import qualified Servant.Server as SS
import qualified Servant.Server.Generic as GSS
import qualified Network.Wai.Middleware.RequestLogger as Wai.Logger
import qualified Data.Aeson.Encode.Pretty as JSON.Pretty
import qualified Data.Time.Format as Time.Fmt
import qualified Control.Exception.Safe as Exc

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
      Nothing -> Exc.throwIO $ SS.err400 {SS.errBody = "Missing required parameter: lease_seconds"}
      Just (HubLeaseSeconds leaseSeconds) -> do
        logStr $ "subscribing to topic: " <> show topic
        now <- liftIO Clk.getCurrentTime
        let expiresAt = Clk.addUTCTime (fromIntegral leaseSeconds) now
        let newLease = Lease {leaseTopic = topic, leaseExpiresAt = expiresAt}
        liftIO $ MVar.modifyMVar_ leases $ \ls -> pure (newLease : ls)
        pure challenge

notificationHandler ::
  Chan.TBMChan StreamNotification ->
  StreamNotification ->
  SS.Handler SAPI.NoContent
notificationHandler chan notification = do
  liftIO $ STM.atomically $ Chan.writeTBMChan chan notification
  pure SAPI.NoContent

  -- StreamOffline -> error "wip notification handler"
  -- StreamOnline streamData -> error "wip notification handler"

webhookServerRecord :: ClientEnv -> WebhookRoutes GSS.AsServer
webhookServerRecord env = WebhookRoutes
  { callback = callbackHandler (ceNotificationLeases env),
    notification = notificationHandler (ceNotifChan env)
  }

runWebhookServer :: ClientEnv -> IO ()
runWebhookServer env = do
  let app = GSS.genericServe $ webhookServerRecord env
  Warp.run (ceWebhookServerPort env) (Wai.Logger.logStdout app)

iso8601 :: Clk.UTCTime -> String
iso8601 = Time.Fmt.formatTime Time.Fmt.defaultTimeLocale "%FT%T%QZ"

logStr :: (MonadIO m) => String -> m ()
logStr msg = liftIO $ do
  now <- Clk.getCurrentTime
  putStr $ iso8601 now <> " " <> msg <> "\n"

-- watchStreams

-- test :: IO ()
-- test =
--   void $
--     Async.concurrently
--       (subscribeToChannel 77066482)
--       (runWebhookServer 8888)
