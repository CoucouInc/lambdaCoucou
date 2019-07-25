{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module LambdaCoucou.Client where

import           Control.Applicative
import           Control.Lens                 ((%=), (%~), (&), (.~), (<+=))
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


test :: IO ()
test = do
  let tlsConfig = IRC.C.WithDefaultConfig "chat.freenode.net" 6697
  let connectionConfig = IRC.C.tlsConnection tlsConfig
        & IRC.L.logfunc .~ IRC.C.stdoutLogger
  let instanceConfig = IRC.C.defaultInstanceConfig "lambdatest"
        & IRC.L.channels .~ ["#gougoutest"]
        & IRC.L.version .~ "lambdacoucou-v2"
        & IRC.L.handlers %~ (
          [ updateLastUrlHandler
          , commandHandler
          , LC.Date.ctcpTimeHandler
          , testEventHandler
          ] ++)
  ytApiKey <- LC.St.YoutubeAPIKey . Tx.pack <$> Env.getEnv "YT_API_KEY"
  IRC.C.runClient connectionConfig instanceConfig (LC.St.initialState ytApiKey)
  putStrLn "exiting"

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
      let lastUrl = LC.P.parseUrl msg
      field @"csLastUrl" %= (\old -> lastUrl <|> old)
      _newCounter <- field @"csCounter" <+= 1
      st <- St.get
      let resp = "state: " <> show st
      liftIO $ putStrLn $ "from: " <> show source <> " - " <> resp
      IRC.C.replyTo source (Tx.pack resp)
    _ -> pure ()
  )

commandHandler :: IRC.Ev.EventHandler LC.St.CoucouState
commandHandler = IRC.Ev.EventHandler
  (IRC.Ev.matchType IRC.Ev._Privmsg)
  (\source (_target, raw) -> case (source, raw) of
    -- only ignore bots for this handler. A url produced by another bot
    -- should still trigger updateLastUrlHandler
    (IRC.Ev.Channel _ nick, Right msg) -> unless (blacklisted nick) $
      case LC.P.parseCommand msg of
        Left _err -> pure ()
        Right cmd -> do
          liftIO $ putStrLn $ "handling command: " <> show cmd
          execCommand cmd >>= replyTo source
    _ -> pure ()
  )


replyTo :: IRC.Ev.Source Text -> Maybe Text -> IRC.C.IRC s ()
replyTo source = maybe (pure ()) (IRC.C.replyTo source)

blacklisted :: Text -> Bool
blacklisted nick = nick `elem` ["coucoubot", "zoe_bot", "M`arch`ov"]

execCommand :: LC.Cmd.CoucouCmd -> IRC.C.IRC LC.St.CoucouState (Maybe Text)
execCommand = \case
  LC.Cmd.Nop -> pure Nothing
  LC.Cmd.Url mbTarget -> LC.Url.fetchUrlCommandHandler mbTarget
  LC.Cmd.Crypto coin target -> LC.C.cryptoCommandHandler coin target
  LC.Cmd.Date target -> LC.Date.dateCommandHandler target
  LC.Cmd.Cancer cancer target -> do
    reply <- LC.Cancer.cancerCommandHandler cancer target
    -- a cancer command will produce a url
    case reply of
      Nothing -> pure ()
      Just x  -> do
        liftIO $ putStrLn $ "updating last url from: " <> show reply <> " - " <> show (LC.P.parseUrl x)
        updateLastUrl x
    pure reply

addTarget :: Maybe Text -> Text -> Text
addTarget mbTarget msg = case mbTarget of
  Nothing -> msg
  Just x  -> x <> ": " <> msg

updateLastUrlHandler :: IRC.Ev.EventHandler LC.St.CoucouState
updateLastUrlHandler = IRC.Ev.EventHandler
  (IRC.Ev.matchType IRC.Ev._Privmsg)
  (\source (_target, raw) -> case (source, raw) of
    (IRC.Ev.Channel _ _, Right msg) ->
      updateLastUrl msg
    _ -> pure ()
  )

updateLastUrl :: (St.MonadState LC.St.CoucouState m) => Text -> m ()
updateLastUrl msg = field @"csLastUrl" %= (LC.P.parseUrl msg <|>)
