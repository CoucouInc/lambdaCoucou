{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module LambdaCoucou.Client where

import           Control.Lens                 ((&), (.~), (<+=))
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import qualified Control.Monad.State.Strict   as St
import           Data.Generics.Product.Fields (field)
import           Data.Text                    (Text)
import qualified Data.Text                    as Tx
import qualified Network.IRC.Client           as IRC.C
import qualified Network.IRC.Client.Events    as IRC.Ev
import qualified Network.IRC.Client.Lens      as IRC.L
import qualified System.Environment           as Env

import qualified LambdaCoucou.Cancer          as LC.Cancer
import qualified LambdaCoucou.Command         as LC.Cmd
import qualified LambdaCoucou.Crypto          as LC.C
import qualified LambdaCoucou.Date            as LC.Date
import qualified LambdaCoucou.Parser          as LC.P
import qualified LambdaCoucou.State           as LC.St
import qualified LambdaCoucou.Url             as LC.Url
import qualified LambdaCoucou.Channel             as LC.Chan


test :: IO ()
test = do
  let tlsConfig = IRC.C.WithDefaultConfig "chat.freenode.net" 6697
  let connectionConfig = IRC.C.tlsConnection tlsConfig
        & IRC.L.logfunc .~ IRC.C.stdoutLogger
  let instanceConfig = IRC.C.defaultInstanceConfig "foolambdatest"
        & IRC.L.channels .~ ["#gougoutest"]
        & IRC.L.version .~ "lambdacoucou-v2"
        & IRC.L.handlers .~ handlers
  ytApiKey <- LC.St.YoutubeAPIKey . Tx.pack <$> Env.getEnv "YT_API_KEY"
  IRC.C.runClient connectionConfig instanceConfig (LC.St.initialState ytApiKey)
  putStrLn "exiting"

handlers :: [IRC.Ev.EventHandler LC.St.CoucouState]
handlers =
  [ LC.Url.updateLastUrlHandler
  , LC.Date.ctcpTimeHandler
  , testEventHandler
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

-- test :: IO ()
-- test = do
--   list <- LC.Cancer.runFetch LC.Cancer.fetchCancerList
--   print list
--   pure ()

testEventHandler :: IRC.Ev.EventHandler LC.St.CoucouState
testEventHandler = IRC.Ev.EventHandler
  (IRC.Ev.matchType IRC.Ev._Privmsg)
  (\source (_target, raw) -> case (source, raw) of
    (IRC.Ev.Channel _ _, Right msg) -> do
      liftIO $ putStrLn "testevent handler"
      -- let lastUrl = LC.P.parseUrl msg
      -- field @"csLastUrl" %= (\old -> lastUrl <|> old)
      _newCounter <- field @"csCounter" <+= 1
      st <- St.get
      let resp = "state: " <> show st
      when (msg == "state") $ do
        liftIO $ putStrLn $ "from: " <> show source <> " - " <> resp
        IRC.C.replyTo source (Tx.pack resp)

      when (msg == "nick") $ do
        instanceCfg <- IRC.C.getIRCState >>= IRC.C.snapshot IRC.C.instanceConfig
        let ownNick = instanceCfg ^. IRC.C.nick
        when (ownNick == "foolambdatest") $ IRC.C.setNick "foolambdatestBAR"
        when (ownNick == "foolambdatestBAR") $ IRC.C.setNick "foolambdatest"

      when (msg == "part") $ do
        instanceCfg <- IRC.C.getIRCState >>= IRC.C.snapshot IRC.C.instanceConfig
        let ownNick = instanceCfg ^. IRC.C.nick
        when (ownNick == "foolambdatestBAR") $
          IRC.C.leaveChannel "#gougoutest" (Just "I'll be back")

    _ -> pure ()
  )

commandHandler :: IRC.Ev.EventHandler LC.St.CoucouState
commandHandler = IRC.Ev.EventHandler
  (IRC.Ev.matchType IRC.Ev._Privmsg)
  (\source (_target, raw) -> case (source, raw) of
    -- only ignore bots for this handler. A url produced by another bot
    -- should still trigger updateLastUrlHandler
    (IRC.Ev.Channel chanName nick, Right msg) -> unless (blacklisted nick) $
      case LC.P.parseCommand msg of
        Left _err -> pure ()
        Right cmd -> do
          liftIO $ putStrLn $ "handling command: " <> show cmd
          execCommand (LC.St.ChannelName chanName) cmd >>= replyTo source
    _ -> pure ()
  )


replyTo :: IRC.Ev.Source Text -> Maybe Text -> IRC.C.IRC s ()
replyTo source = maybe (pure ()) (IRC.C.replyTo source)

blacklisted :: Text -> Bool
blacklisted nick = nick `elem` ["coucoubot", "zoe_bot", "M`arch`ov"]

execCommand
  :: LC.St.ChannelName
  -> LC.Cmd.CoucouCmd
  -> IRC.C.IRC LC.St.CoucouState (Maybe Text)

execCommand chanName = \case
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

addTarget :: Maybe Text -> Text -> Text
addTarget mbTarget msg = case mbTarget of
  Nothing -> msg
  Just x  -> x <> ": " <> msg
