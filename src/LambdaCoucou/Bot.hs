module LambdaCoucou.Bot where

import RIO
import System.IO (putStrLn)

import qualified Control.Concurrent.Async  as Async
import           Control.Lens              ((&), (.~))
import qualified GHC.Conc                  as TVar

import qualified Network.IRC.Client        as IRC.C
import qualified Network.IRC.Client.Events as IRC.Ev
import qualified Network.IRC.Client.Lens   as IRC.L

import qualified LambdaCoucou.Cancer       as LC.Cancer
import qualified LambdaCoucou.Channel      as LC.Chan
import qualified LambdaCoucou.Cli          as LC.Cli
import qualified LambdaCoucou.Command      as LC.Cmd
import qualified LambdaCoucou.Crypto       as LC.C
import qualified LambdaCoucou.CryptoMonitor as LC.Mon
import qualified LambdaCoucou.Date         as LC.Date
import qualified LambdaCoucou.Debug        as LC.Dbg
import qualified LambdaCoucou.Help         as LC.Hlp
import qualified LambdaCoucou.Parser       as LC.P
import qualified LambdaCoucou.PR           as LC.PR
import qualified LambdaCoucou.State        as LC.St
import qualified LambdaCoucou.Url          as LC.Url
import qualified LambdaCoucou.Joke         as LC.Joke
import qualified LambdaCoucou.Twitch         as LC.Twitch

runBot :: IO ()
runBot = do
  config <- LC.Cli.getConfig
  let tlsConfig = IRC.C.WithDefaultConfig "chat.freenode.net" 6697
  let connectionConfig = IRC.C.tlsConnection tlsConfig
        & IRC.L.logfunc .~ IRC.C.stdoutLogger
  let instanceConfig = IRC.C.defaultInstanceConfig (LC.Cli.nick config)
        & IRC.L.channels .~ [LC.Cli.chan config]
        & IRC.L.version .~ "lambdacoucou-v2"
        & IRC.L.handlers .~ handlers
  ircState <- IRC.C.newIRCState connectionConfig instanceConfig
    (LC.St.initialState (LC.Cli.ytApiKey config) (LC.Cli.sqlitePath config))

  IRC.C.runIRCAction (IRC.C.fork $ LC.Mon.monitorRates (LC.Cli.sqlitePath config)) ircState

  void $ Async.race
    (LC.Twitch.watchStreams ircState)
    (IRC.C.runClientWith ircState)
  putStrLn "exiting"

handlers :: [IRC.Ev.EventHandler LC.St.CoucouState]
handlers =
  [ LC.Url.updateLastUrlHandler
  , LC.Date.ctcpTimeHandler
  , LC.Dbg.debugEventHandler
  , commandHandler
  ]
  ++ LC.Chan.channelStateHandlers

  -- following are IRC.Ev.defaultEventHandlers without the ctcpTimeHandler
  ++
  [ IRC.Ev.pingHandler
  , IRC.Ev.kickHandler
  , IRC.Ev.ctcpPingHandler
  -- , IRC.Ev.ctcpTimeHandler
  -- ^ replaced by the republican ctcpTimeHandler
  , IRC.Ev.ctcpVersionHandler
  , IRC.Ev.welcomeNick
  , IRC.Ev.joinOnWelcome
  , IRC.Ev.joinHandler
  , IRC.Ev.nickMangler
  ]


commandHandler :: IRC.Ev.EventHandler LC.St.CoucouState
commandHandler = IRC.Ev.EventHandler
  (IRC.Ev.matchType IRC.Ev._Privmsg)
  (\source (_target, raw) -> case (source, raw) of
    -- only ignore bots for this handler. A url produced by another bot
    -- should still trigger updateLastUrlHandler
    (IRC.Ev.Channel chanName nick, Right msg) -> unless (blacklisted nick) $ do
      instanceCfg <- asks (^. IRC.C.instanceConfig)
      ownNick <- (^. IRC.C.nick) <$> liftIO (TVar.readTVarIO instanceCfg)
      if msg == "coucou " <> ownNick
        then replyTo source (Just $ "coucou " <> nick)
        else case LC.P.parseCommand msg of
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

execCommand
  :: LC.St.ChannelName
  -> Text
  -> LC.Cmd.CoucouCmd
  -> IRC.C.IRC LC.St.CoucouState (Maybe Text)

execCommand chanName nick = \case
  LC.Cmd.Nop -> pure Nothing
  LC.Cmd.Url mbTarget -> LC.Url.fetchUrlCommandHandler mbTarget
  LC.Cmd.Crypto coin target -> LC.C.cryptoCommandHandler coin target
  LC.Cmd.Date target -> LC.Date.dateCommandHandler target
  LC.Cmd.Cancer cancer target -> do
    reply <- LC.Cancer.cancerCommandHandler cancer target
    -- a cancer command will produce a url
    case reply of
      Nothing -> pure ()
      Just x  -> LC.Url.updateLastUrl x
    pure reply
  LC.Cmd.ShoutCoucou -> LC.Chan.shoutCoucouCommandHandler chanName
  LC.Cmd.HeyCoucou -> pure $ Just $ "écoucou " <> nick
  LC.Cmd.PR target -> LC.PR.prCommandHandler target
  LC.Cmd.Help hlpCmd target -> LC.Hlp.helpCommandHandler hlpCmd target
  LC.Cmd.Joke target -> LC.Joke.jokeCommandHandler target

addTarget :: Maybe Text -> Text -> Text
addTarget mbTarget msg = case mbTarget of
  Nothing -> msg
  Just x  -> x <> ": " <> msg
