module LambdaCoucou.Twitch where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM.TBMChan as Chan
import qualified Control.Monad.STM as STM
import qualified Data.Aeson as JSON
import qualified LambdaCoucou.HandlerUtils as LC.Hdl
import qualified LambdaCoucou.Http as LC.HTTP
import qualified LambdaCoucou.State as LC.St
import qualified LambdaCoucou.TwitchHook as Hook
import LambdaCoucou.TwitchTypes
import qualified LambdaCoucou.Url as LC.Url
import Network.HTTP.Req ((/:), (=:))
import qualified Network.HTTP.Req as Req
import qualified Network.IRC.Client as IRC.C
import qualified Network.IRC.Client.Events as IRC.Ev
import RIO
import qualified RIO.Set as Set
import qualified RIO.State as St
import qualified RIO.Text as T
import qualified RIO.Time as Time
import qualified RIO.Vector as V
import Say
import qualified System.Environment as Env
import qualified UnliftIO.Async as Async

getClientCredentials ::
  (MonadIO m, MonadThrow m) =>
  ClientID ->
  ClientSecret ->
  m ClientCredentials
getClientCredentials clientID clientSecret = do
  let options =
        "client_id" =: clientID
          <> "client_secret" =: clientSecret
          <> "grant_type" =: ("client_credentials" :: Text)
  logStr "Getting twitch credentials"
  resp <-
    LC.HTTP.runFetchEx $
      Req.req
        Req.POST
        (Req.https "id.twitch.tv" /: "oauth2" /: "token")
        Req.NoReqBody
        Req.jsonResponse
        options
  now <- liftIO Time.getCurrentTime
  let ccr = Req.responseBody resp
  let expiresAt = Time.addUTCTime (fromIntegral $ ccrExpiresInSeconds ccr) now
  logStr $ "Got twitch credentials. Expire at: " <> show expiresAt
  pure $
    ClientCredentials
      { ccClientAuthToken = ccrClientAuthToken ccr,
        ccExpiresAt = expiresAt
      }

-- | returns a valid auth token. If the existing token is nearing expiration, it will be
-- replaced and the updated token will be returned
ensureClientToken :: MonadIO m => ClientEnv -> m ClientAuthToken
ensureClientToken ClientEnv {ceClientID, ceClientSecret, ceClientCredentials} =
  liftIO $
    MVar.modifyMVar ceClientCredentials $ \case
      Nothing -> renewCreds
      Just creds -> do
        now <- Time.getCurrentTime
        if now >= Time.addUTCTime 10 (ccExpiresAt creds)
          then renewCreds
          else pure (Just creds, ccClientAuthToken creds)
  where
    renewCreds = do
      newCreds <- getClientCredentials ceClientID ceClientSecret
      pure (Just newCreds, ccClientAuthToken newCreds)

-- Note: this doesn't handle pagination at all.
-- But for this bot's usecase, there is no need for it.
getTwitchAPI ::
  (MonadIO m, MonadThrow m, JSON.FromJSON a) =>
  ClientEnv ->
  Text ->
  Req.Option 'Req.Https ->
  m (Req.JsonResponse a)
getTwitchAPI env urlPath options = do
  token <- ensureClientToken env
  LC.HTTP.runFetchEx $
    Req.req
      Req.GET
      (Req.https "api.twitch.tv" /: "helix" /: urlPath)
      Req.NoReqBody
      Req.jsonResponse
      ( Req.oAuth2Bearer (T.encodeUtf8 $ getClientAuthToken token)
          <> Req.header "Client-ID" (T.encodeUtf8 $ getClientID $ ceClientID env)
          <> options
      )

getTwitchStream :: (MonadIO m, MonadThrow m) => ClientEnv -> UserLogin -> m StreamData
getTwitchStream env (UserLogin userLogin) = do
  resp <- getTwitchAPI env "streams" ("user_login" =: userLogin)
  pure $ getSingleTwitchResponse $ Req.responseBody resp

-- | returns a possibly empty vector of streams currently live from the given list of user id
getLiveStreams :: (MonadIO m, MonadThrow m, Foldable t) => ClientEnv -> t UserId -> m (Vector StreamData)
getLiveStreams env ids = do
  let query = foldMap (\i -> "user_id" =: getUserId i) ids
  resp <- getTwitchAPI env "streams" query
  pure $ gsrData $ Req.responseBody resp

getTwitchUser :: (MonadIO m, MonadThrow m) => ClientEnv -> UserLogin -> m User
getTwitchUser env (UserLogin userLogin) = do
  resp <- getTwitchAPI env "users" ("login" =: userLogin)
  pure $ getSingleTwitchResponse $ Req.responseBody resp

subscribeToChannel :: ClientEnv -> UserId -> IO ()
subscribeToChannel env (UserId userID) =
  postWebhook Subscribe env (HubTopic $ "https://api.twitch.tv/helix/streams?user_id=" <> userID)

unsubscribeFromChannel :: ClientEnv -> UserId -> IO ()
unsubscribeFromChannel env (UserId userID) =
  postWebhook Unsubscribe env (HubTopic $ "https://api.twitch.tv/helix/streams?user_id=" <> userID)

postWebhook :: HubMode -> ClientEnv -> HubTopic -> IO ()
postWebhook hubMode env topic = do
  let lease = HubLeaseSeconds $ 3600 * 24 * 5
  let callbackUrl :: Text = "https://irc.geekingfrog.com/twitch/notifications"
  let payload =
        SubscribeParams
          { spCallback = callbackUrl,
            spMode = hubMode,
            spTopic = topic,
            spLease = lease
          }
  token <- ensureClientToken env
  void $
    LC.HTTP.runFetchEx $
      Req.req
        Req.POST
        (Req.https "api.twitch.tv" /: "helix" /: "webhooks" /: "hub")
        (Req.ReqBodyJson payload)
        Req.bsResponse
        ( Req.oAuth2Bearer (T.encodeUtf8 $ getClientAuthToken token)
            <> Req.header "Client-ID" (T.encodeUtf8 $ getClientID $ ceClientID env)
        )
  logStr $ "subscription request sent: " <> show payload

startWatchChannel :: ClientEnv -> StreamWatcherSpec -> IO ()
startWatchChannel env spec = do
  twitchUser <- liftIO $ getTwitchUser env (swsTwitchUserLogin spec)
  unsubscribeFromChannel env (usrID twitchUser)
  threadDelay (5 * 10 ^ 6)
  subscribeToChannel env (usrID twitchUser)
  logStr $ "subscribed to channel for twitch user: " <> show twitchUser
  pure ()

makeClientEnv :: IO ClientEnv
makeClientEnv = do
  clientID <- ClientID . T.pack <$> getEnv "TWITCH_CLIENT_ID"
  clientSecret <- ClientSecret . T.pack <$> getEnv "TWITCH_CLIENT_SECRET"
  Just port <- readMaybe <$> getEnv "TWITCH_WEBHOOK_SERVER_PORT"
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
    getEnv e =
      Env.lookupEnv e >>= \case
        Nothing -> throwString $ "Missing " <> e
        Just s -> pure s

processNotifications ::
  [StreamWatcherSpec] ->
  Chan.TBMChan StreamNotification ->
  IRC.C.IRC LC.St.CoucouState ()
processNotifications specs chan = loop
  where
    loop =
      liftIO (STM.atomically (Chan.readTBMChan chan)) >>= \case
        Nothing -> logStr "notification channel closed"
        Just notif -> do
          logStr $ "got a notification: " <> show notif
          case notif of
            StreamOffline -> loop
            StreamOnline streamData ->
              case filter
                ( \s ->
                    CaseInsensitive (swsTwitchUserLogin s)
                      == CaseInsensitive (sdUserName streamData)
                )
                specs of
                [] -> loop
                (x : _) -> do
                  let streamUrl = "https://www.twitch.tv/" <> getUserLogin (sdUserName streamData)
                  let str = "Le stream de " <> swsIRCNick x <> " est maintenant live ! " <> streamUrl
                  let chan = swsIRCChan x
                  let msg = IRC.Ev.Privmsg chan (Right str)
                  IRC.C.send msg
                  LC.Url.updateLastUrl (LC.St.ChannelName chan) streamUrl
                  loop

-- | every 10 minutes, check if leases are expiring soon, if yes, renew them
watchLeases :: ClientEnv -> IO ()
watchLeases env = forever $ do
  threadDelay (intervalInSeconds * 10 ^ 6)
  now <- Time.getCurrentTime
  leasesSoonToExpire <- filter (leaseExpiresAfter now intervalInSeconds) <$> MVar.readMVar envLeases
  forM_ leasesSoonToExpire $ \l -> do
    logStr $ "Renewing lease: " <> show l
    postWebhook Subscribe env (leaseTopic l)
  when (null leasesSoonToExpire) $ logStr "no twitch lease to renew"
  where
    envLeases = ceNotificationLeases env
    intervalInSeconds = 60 * 10

leaseExpiresAfter :: Time.UTCTime -> Int -> Lease -> Bool
leaseExpiresAfter now intervalInSeconds lease =
  Time.addUTCTime (fromIntegral intervalInSeconds) now >= leaseExpiresAt lease

-- To get the user_id from a userLogin:
-- curl -H "Client-ID: $TWITCH_CLIENT_ID" -H "Authorization: Bearer $TOKEN" "https://api.twitch.tv/helix/users?login=<USER_LOGIN>"
watchedStreams :: [StreamWatcherSpec]
watchedStreams =
  [ StreamWatcherSpec
      { swsTwitchUserLogin = UserLogin "artart78",
        swsIRCNick = "artart78",
        swsIRCChan = "##arch-fr-free",
        swsUserId = UserId "480815392"
      },
    StreamWatcherSpec
      { swsTwitchUserLogin = UserLogin "gikiam",
        swsIRCNick = "gikiam",
        swsIRCChan = "##arch-fr-free",
        swsUserId = UserId "77066482"
      },
    StreamWatcherSpec
      { swsTwitchUserLogin = UserLogin "geekingfrog",
        swsIRCNick = "Geekingfrog",
        swsIRCChan = "##arch-fr-free",
        swsUserId = UserId "482889678"
      },
    StreamWatcherSpec
      { swsTwitchUserLogin = UserLogin "shampooingonthemove",
        swsIRCNick = "Shampooing",
        swsIRCChan = "##arch-fr-free",
        swsUserId = UserId "535066880"
      },
    StreamWatcherSpec
      { swsTwitchUserLogin = UserLogin "VertBrocoli",
        swsIRCNick = "Armael",
        swsIRCChan = "##arch-fr-free",
        swsUserId = UserId "64172886"
      },
    StreamWatcherSpec
      { swsTwitchUserLogin = UserLogin "therealbarul",
        swsIRCNick = "barul",
        swsIRCChan = "##arch-fr-free",
        swsUserId = UserId "597231373"
      },
    StreamWatcherSpec
      { swsTwitchUserLogin = UserLogin "juantitor",
        swsIRCNick = "JuanTitor",
        swsIRCChan = "##arch-fr-free",
        swsUserId = UserId "42481408"
      }
  ]

watchStreams ::
  IRC.C.IRCState LC.St.CoucouState ->
  MVar.MVar (Maybe ClientEnv) ->
  IO ()
watchStreams botState clientEnvMvar = do
  env <- liftIO makeClientEnv
  MVar.swapMVar clientEnvMvar (Just env)
  Async.runConc $
    Async.conc (IRC.C.runIRCAction (processNotifications watchedStreams (ceNotifChan env)) botState)
      <> Async.conc (traverse_ (startWatchChannel env) watchedStreams)
      <> Async.conc (Hook.runWebhookServer env)
      <> Async.conc (watchLeases env)
  logStr "Not watching streams anymore"

iso8601 :: Time.UTCTime -> String
iso8601 = Time.formatTime Time.defaultTimeLocale "%FT%T%QZ"

logStr :: (MonadIO m) => String -> m ()
logStr msg = liftIO $ do
  now <- Time.getCurrentTime
  sayString $ iso8601 now <> " " <> msg

processTest :: ClientEnv -> IO ()
processTest env = loop
  where
    loop =
      STM.atomically (Chan.readTBMChan $ ceNotifChan env) >>= \case
        Nothing -> logStr "channel closed"
        Just n -> do
          logStr $ "got notification:" <> show n
          loop

liveStreamsCommandHandler ::
  LC.St.ChannelName ->
  Maybe Text ->
  IRC.C.IRC LC.St.CoucouState (Maybe Text)
liveStreamsCommandHandler (LC.St.ChannelName chanName) mbTarget = do
  st <- St.gets LC.St.csTwitch
  liftIO (MVar.readMVar st) >>= \case
    Nothing -> pure $ Just $ LC.Hdl.addTarget mbTarget "Twitch module désactivé."
    Just env -> do
      allLiveStreams <- getLiveStreams env (map swsUserId watchedStreams)
      let chanStreams =
            Set.fromList $
              map swsUserId $ filter ((== chanName) . swsIRCChan) watchedStreams
      let chanLiveStreams = V.filter (\s -> sdUserId s `Set.member` chanStreams) allLiveStreams
      pure $
        Just $
          LC.Hdl.addTarget mbTarget $
            if null chanLiveStreams
              then "Y'a personne qui stream ici çaynul !"
              else "Stream(s) live: " <> (T.intercalate ", " $ V.toList $ V.map format chanLiveStreams)
  where
    format :: StreamData -> Text
    format sd = "https://www.twitch.tv/" <> getUserLogin (sdUserName sd) <> " commencé à " <> sdStartedAt sd

test :: IO ()
test = do
  clientID <- ClientID . T.pack <$> Env.getEnv "TWITCH_CLIENT_ID"
  clientSecret <- ClientSecret . T.pack <$> Env.getEnv "TWITCH_CLIENT_SECRET"
  getClientCredentials clientID clientSecret >>= logStr . show
  -- error "coucou"
  env <- makeClientEnv

  let specs =
        [ StreamWatcherSpec
            { swsTwitchUserLogin = UserLogin "geekingfrog",
              swsIRCNick = "geekingfrog",
              swsIRCChan = "#gougoutest",
              swsUserId = UserId "482889678"
            }
        ]
  Async.runConc $
    Async.conc (traverse_ (startWatchChannel env) specs)
      <> Async.conc (processTest env)
      <> Async.conc (Hook.runWebhookServer env)
  -- getClientCredentials clientID clientSecret >>= print
  -- getTwitchStream clientAuthToken stream >>= print
  -- getTwitchUser clientAuthToken (UserLogin "gikiam") >>= print
  logStr "test all done"
