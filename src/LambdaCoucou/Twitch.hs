{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module LambdaCoucou.Twitch where

import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM.TBMChan as Chan
import qualified Control.Exception.Safe as Exc
import Control.Monad.IO.Class
import qualified Control.Monad.STM as STM
import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import Data.Foldable
import Data.Functor (void)
import qualified Data.Text as Tx
import qualified Data.Text.Encoding as Tx
import qualified Data.Text.IO as Tx.IO
import qualified Data.Time.Clock as Clk
import qualified Data.Time.Format as Time.Fmt
import qualified Data.Vector as V
import qualified LambdaCoucou.Http as LC.HTTP
import qualified LambdaCoucou.TwitchHook as Hook
import LambdaCoucou.TwitchTypes
import Network.HTTP.Req ((/:), (=:))
import qualified Network.HTTP.Req as Req
import qualified Network.IRC.Client as IRC.C
import qualified Network.IRC.Client.Events as IRC.Ev
import qualified Servant as S
import qualified Servant.API as SAPI
import qualified System.Environment as Env
import qualified System.IO as Sys.IO

getClientCredentials ::
  (MonadIO m, Exc.MonadThrow m) =>
  ClientID ->
  ClientSecret ->
  m ClientCredentials
getClientCredentials clientID clientSecret = do
  let options =
        "client_id" =: clientID
          <> "client_secret" =: clientSecret
          <> "grant_type" =: ("client_credentials" :: Tx.Text)
  resp <-
    LC.HTTP.runFetchEx $
      Req.req
        Req.POST
        (Req.https "id.twitch.tv" /: "oauth2" /: "token")
        Req.NoReqBody
        Req.jsonResponse
        options
  now <- liftIO Clk.getCurrentTime
  let ccr = Req.responseBody resp
  let expiresAt = Clk.addUTCTime (fromIntegral $ ccrExpiresInSeconds ccr) now
  pure $
    ClientCredentials
      { ccClientAuthToken = ccrClientAuthToken ccr,
        ccExpiresAt = expiresAt
      }

-- | returns a valid auth token. If the existing token is nearing expiration, it will be
-- replaced and the updated token will be returned
ensureClientToken :: ClientEnv -> IO ClientAuthToken
ensureClientToken env@ClientEnv {ceClientID, ceClientSecret, ceClientCredentials} =
  MVar.modifyMVar ceClientCredentials $ \case
    Nothing -> renewCreds
    Just creds -> do
      now <- Clk.getCurrentTime
      if now >= Clk.addUTCTime 10 (ccExpiresAt creds)
        then renewCreds
        else pure (Just creds, ccClientAuthToken creds)
  where
    renewCreds = do
      newCreds <- getClientCredentials ceClientID ceClientSecret
      pure (Just newCreds, ccClientAuthToken newCreds)

-- hardcode that for now
clientAuthToken :: ClientAuthToken
clientAuthToken = ClientAuthToken "vb3uprkwb69xit08aw9thtj4zr98qy"

getSingleTwitchData ::
  (MonadIO m, Exc.MonadThrow m, JSON.FromJSON a) =>
  Tx.Text ->
  ClientAuthToken ->
  Req.Option Req.Https ->
  m a
getSingleTwitchData urlPath token options = do
  resp <-
    LC.HTTP.runFetchEx $
      Req.req
        Req.GET
        (Req.https "api.twitch.tv" /: "helix" /: urlPath)
        Req.NoReqBody
        Req.jsonResponse
        (Req.oAuth2Bearer (Tx.encodeUtf8 $ getClientAuthToken token) <> options)
  pure $ getSingleTwitchResponse $ Req.responseBody resp

getTwitchStream :: (MonadIO m, Exc.MonadThrow m) => ClientAuthToken -> UserLogin -> m StreamData
getTwitchStream token (UserLogin userLogin) =
  getSingleTwitchData "streams" token ("user_login" =: userLogin)

getTwitchUser :: (MonadIO m, Exc.MonadThrow m) => ClientAuthToken -> UserLogin -> m User
getTwitchUser token (UserLogin userLogin) =
  getSingleTwitchData "users" token ("login" =: userLogin)

subscribeToChannel :: ClientEnv -> UserID -> IO ()
subscribeToChannel env (UserID userID) =
  postWebhook Subscribe env (HubTopic $ "https://api.twitch.tv/helix/streams?user_id=" <> userID)

unsubscribeFromChannel :: ClientEnv -> UserID -> IO ()
unsubscribeFromChannel env (UserID userID) =
  postWebhook Unsubscribe env (HubTopic $ "https://api.twitch.tv/helix/streams?user_id=" <> userID)

postWebhook :: HubMode -> ClientEnv -> HubTopic -> IO ()
postWebhook hubMode env topic = do
  let lease = HubLeaseSeconds $ 3600 * 24 * 5
  let callbackUrl :: Tx.Text = "https://irc.geekingfrog.com/twitch/notifications"
  let payload =
        SubscribeParams
          { spCallback = callbackUrl,
            spMode = hubMode,
            spTopic = topic,
            spLease = lease
          }
  token <- ensureClientToken env
  void $ LC.HTTP.runFetchEx $
    Req.req
      Req.POST
      (Req.https "api.twitch.tv" /: "helix" /: "webhooks" /: "hub")
      (Req.ReqBodyJson payload)
      Req.bsResponse
      (Req.oAuth2Bearer (Tx.encodeUtf8 $ getClientAuthToken token))
  logStr $ "subscription request sent: " <> show payload

startWatchChannel :: ClientEnv -> StreamWatcherSpec -> IO ()
startWatchChannel env spec = do
  token <- ensureClientToken env
  twitchUser <- getTwitchUser token (swsTwitchUserLogin spec)
  unsubscribeFromChannel env (usrID twitchUser)
  Conc.threadDelay (5 * 10 ^ 6)
  subscribeToChannel env (usrID twitchUser)
  logStr $ "subscribed to channel for twitch user: " <> show twitchUser
  pure ()

makeClientEnv :: IO ClientEnv
makeClientEnv = do
  clientID <- ClientID . Tx.pack <$> getEnv "TWITCH_CLIENT_ID"
  clientSecret <- ClientSecret . Tx.pack <$> getEnv "TWITCH_CLIENT_SECRET"
  port <- read <$> getEnv "TWITCH_WEBHOOK_SERVER_PORT"
  creds <- MVar.newMVar Nothing
  leases <- MVar.newMVar []
  chan <- Chan.newTBMChanIO 1
  pure $
    ClientEnv
      { ceClientID = clientID,
        ceClientSecret = clientSecret,
        ceClientCredentials = creds,
        ceWebhookServerPort = port,
        ceNotificationLeases = leases,
        ceNotifChan = chan
      }
  where
    getEnv e = Env.lookupEnv e >>= \case
      Nothing -> Exc.throwString $ "Missing " <> e
      Just s -> pure s

processNotifications :: [StreamWatcherSpec] -> Chan.TBMChan StreamNotification -> IRC.C.IRC s ()
processNotifications specs chan = loop
  where
    loop = liftIO (STM.atomically (Chan.readTBMChan chan)) >>= \case
      Nothing -> logStr "notification channel closed"
      Just notif -> do
        logStr $ "got a notification: " <> show notif
        case notif of
          StreamOffline -> loop
          StreamOnline streamData ->
            case filter (\s -> swsTwitchUserLogin s == sdUserName streamData) specs of
              [] -> loop
              (x : _) -> do
                let streamUrl = "https://www.twitch.tv/" <> getUserLogin (sdUserName streamData)
                let str = "Le stream de " <> swsIRCNick x <> " est maintenant live ! " <> streamUrl
                let msg = IRC.Ev.Privmsg (swsIRCChan x) (Right str)
                IRC.C.send msg
                loop

watchStreams :: IRC.C.IRCState s -> IO ()
watchStreams botState = do
  env <- liftIO makeClientEnv
  let specs =
        [ StreamWatcherSpec
            { swsTwitchUserLogin = UserLogin "artart78",
              swsIRCNick = "artart78",
              swsIRCChan = "#arch-fr-free"
            },
          StreamWatcherSpec
            { swsTwitchUserLogin = UserLogin "gikiam",
              swsIRCNick = "gikiam",
              swsIRCChan = "#arch-fr-free"
            },
          StreamWatcherSpec
            { swsTwitchUserLogin = UserLogin "geekingfrog",
              swsIRCNick = "Geekingfrog",
              swsIRCChan = "#arch-fr-free"
            }
        ]
  Async.runConcurrently $
    (,,)
      <$> Async.Concurrently (IRC.C.runIRCAction (processNotifications specs (ceNotifChan env)) botState)
      <*> Async.Concurrently (traverse_ (startWatchChannel env) specs)
      <*> Async.Concurrently (Hook.runWebhookServer env)
  logStr "Not watching streams anymore"

iso8601 :: Clk.UTCTime -> String
iso8601 = Time.Fmt.formatTime Time.Fmt.defaultTimeLocale "%FT%T%QZ"

logStr :: (MonadIO m) => String -> m ()
logStr msg = liftIO $ do
  now <- Clk.getCurrentTime
  putStr $ iso8601 now <> " " <> msg <> "\n"

processTest :: ClientEnv -> IO ()
processTest env = loop
  where
    loop = STM.atomically (Chan.readTBMChan $ ceNotifChan env) >>= \case
      Nothing -> logStr "channel closed"
      Just n -> do
        logStr $ "got notification:" <> show n
        loop

test :: IO ()
test = do
  env <- makeClientEnv
  -- let specs =
  --       [ StreamWatcherSpec
  --           { swsTwitchUserLogin = UserLogin "artart78",
  --             swsIRCNick = "artart78",
  --             swsIRCChan = "#arch-fr-free"
  --           },
  --         StreamWatcherSpec
  --           { swsTwitchUserLogin = UserLogin "geekingfrog",
  --             swsIRCNick = "geekingfrog",
  --             swsIRCChan = "#gougoutest"
  --           },
  --         StreamWatcherSpec
  --           { swsTwitchUserLogin = UserLogin "gikiam",
  --             swsIRCNick = "gikiam",
  --             swsIRCChan = "#arch-fr-free"
  --           }
  --       ]

  let specs =
        [ StreamWatcherSpec
            { swsTwitchUserLogin = UserLogin "geekingfrog",
              swsIRCNick = "geekingfrog",
              swsIRCChan = "#gougoutest"
            }
        ]
  void $
    Async.concurrently
      ( Async.concurrently
          (traverse_ (startWatchChannel env) specs)
          (processTest env)
      )
      (Hook.runWebhookServer env)
  -- getClientCredentials clientID clientSecret >>= print
  -- getTwitchStream clientAuthToken stream >>= print
  -- getTwitchUser clientAuthToken (UserLogin "gikiam") >>= print
  logStr "test all done"
