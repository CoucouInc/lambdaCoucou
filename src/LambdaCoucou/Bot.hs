module LambdaCoucou.Bot where

import qualified LambdaCoucou.Cancer as LC.Cancer
import qualified LambdaCoucou.Channel as LC.Chan
import qualified LambdaCoucou.Cli as LC.Cli
import qualified LambdaCoucou.Command as LC.Cmd
import qualified LambdaCoucou.Crypto as LC.C
import qualified LambdaCoucou.Date as LC.Date
import qualified LambdaCoucou.Help as LC.Hlp
import qualified LambdaCoucou.Joke as LC.Joke
import qualified LambdaCoucou.CoucouTrain as LC.C.Coucou
import qualified LambdaCoucou.PR as LC.PR
import qualified LambdaCoucou.Parser as LC.P
import qualified LambdaCoucou.State as LC.St
import qualified LambdaCoucou.Remind as LC.Remind
import qualified LambdaCoucou.Twitch as LC.Twitch
import qualified LambdaCoucou.Url as LC.Url
import qualified Network.IRC.Client as IRC.C
import qualified Network.IRC.Client.Events as IRC.Ev
import qualified Network.IRC.Client.Lens as IRC.L
import RIO
import qualified RIO.Text as T
import qualified System.Environment as Env
import System.IO (putStrLn)
import qualified UnliftIO.Async as Async

runBot :: IO ()
runBot = do
  config <- LC.Cli.getConfig
  let tlsConfig = IRC.C.WithDefaultConfig "chat.freenode.net" 6697
  let connectionConfig =
        IRC.C.tlsConnection tlsConfig
          & IRC.L.logfunc .~ IRC.C.stdoutLogger
  let instanceConfig =
        IRC.C.defaultInstanceConfig (LC.Cli.nick config)
          & IRC.L.channels .~ [LC.Cli.chan config]
          & IRC.L.version .~ "lambdacoucou-v2"
          & IRC.L.handlers .~ handlers

  ytKey <-
    Env.lookupEnv "YT_API_KEY" >>= \case
      Just k -> pure $ LC.St.YoutubeAPIKey $ T.pack k
      Nothing -> error "YT_API_KEY not found in environment aborting."

  initialBotState <- LC.St.initialState ytKey (LC.Cli.sqlitePath config)
  ircState <-
    IRC.C.newIRCState
      connectionConfig
      instanceConfig
      initialBotState

  let noopTwitch val = do
        putStrLn $ "TWITCH_MODULE set to " <> show val <> " ≠ 1 -> not watching any streams"
        forever (threadDelay maxBound)
  (twitchProcess :: IO ()) <-
    Env.lookupEnv "TWITCH_MODULE" >>= \env -> case env of
      Just "1" -> pure $ LC.Twitch.watchStreams ircState (LC.St.csTwitch initialBotState)
      _ -> pure $ noopTwitch env

  -- Run all actions in parallel, and if one of them returns, abort everything
  -- It's primitive, but if something goes wrong, that should be somewhat visible
  void $
    Async.runConc
      ( (Async.conc twitchProcess)
          <|> (Async.conc (IRC.C.runClientWith ircState))
          <|> (Async.conc (IRC.C.runIRCAction (LC.C.monitorRates (LC.Cli.sqlitePath config)) ircState))
          <|> (Async.conc (IRC.C.runIRCAction LC.Remind.processReminders ircState))
      )
  putStrLn "exiting"

handlers :: [IRC.Ev.EventHandler LC.St.CoucouState]
handlers =
  [ LC.Url.updateLastUrlHandler,
    LC.Date.ctcpTimeHandler,
    commandHandler
  ]
    ++ LC.Chan.channelStateHandlers
    -- following are IRC.Ev.defaultEventHandlers without the ctcpTimeHandler
    ++ [ IRC.Ev.pingHandler,
         IRC.Ev.kickHandler,
         IRC.Ev.ctcpPingHandler,
         -- , IRC.Ev.ctcpTimeHandler
         -- replaced by the republican ctcpTimeHandler
         IRC.Ev.ctcpVersionHandler,
         IRC.Ev.welcomeNick,
         IRC.Ev.joinOnWelcome,
         IRC.Ev.joinHandler,
         IRC.Ev.nickMangler
       ]

commandHandler :: IRC.Ev.EventHandler LC.St.CoucouState
commandHandler =
  IRC.Ev.EventHandler
    (IRC.Ev.matchType IRC.Ev._Privmsg)
    ( \source (_target, raw) -> case (source, raw) of
        -- only ignore bots for this handler. A url produced by another bot
        -- should still trigger updateLastUrlHandler
        (IRC.Ev.Channel chanName nick, Right msg) -> unless (blacklisted nick) $ do
          LC.C.Coucou.coucouTrainHandler source (LC.St.ChannelName chanName) nick msg
          case LC.P.parseCommand msg of
              Left _err -> pure ()
              Right cmd -> do
                liftIO $ putStrLn $ "handling command: " <> show cmd
                execCommand (LC.St.ChannelName chanName) nick cmd >>= replyTo source

        _ -> pure ()
    )

replyTo :: IRC.Ev.Source Text -> Maybe Text -> IRC.C.IRC s ()
replyTo source = maybe (pure ()) (IRC.C.replyTo source)

-- | a list of other bots to ignore for some commands
blacklisted :: Text -> Bool
blacklisted nick = nick `elem` ["coucoubot", "zoe_bot", "M`arch`ov", "coucoucou"]

execCommand ::
  LC.St.ChannelName ->
  Text ->
  LC.Cmd.CoucouCmd ->
  IRC.C.IRC LC.St.CoucouState (Maybe Text)
execCommand chanName nick = \case
  LC.Cmd.Nop -> pure Nothing
  LC.Cmd.Url offset mbTarget -> LC.Url.fetchUrlCommandHandler chanName offset mbTarget
  LC.Cmd.Crypto coin target -> LC.C.cryptoCommandHandler coin target
  LC.Cmd.Date target -> LC.Date.dateCommandHandler target
  LC.Cmd.Cancer cancer target -> do
    reply <- LC.Cancer.cancerCommandHandler chanName cancer target
    -- a cancer command will produce a url
    case reply of
      Nothing -> pure ()
      Just x -> LC.Url.updateLastUrl chanName x
    pure reply
  LC.Cmd.ShoutCoucou -> LC.Chan.shoutCoucouCommandHandler chanName
  LC.Cmd.HeyCoucou -> pure $ Just $ "écoucou " <> nick
  LC.Cmd.PR target -> LC.PR.prCommandHandler target
  LC.Cmd.Help hlpCmd target -> LC.Hlp.helpCommandHandler hlpCmd target
  LC.Cmd.Joke target -> LC.Joke.jokeCommandHandler target
  LC.Cmd.Remind remindCommand -> LC.Remind.remindCommandHandler chanName nick remindCommand

addTarget :: Maybe Text -> Text -> Text
addTarget mbTarget msg = case mbTarget of
  Nothing -> msg
  Just x -> x <> ": " <> msg
