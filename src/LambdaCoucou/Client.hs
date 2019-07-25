{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module LambdaCoucou.Client where

import           Control.Applicative
import           Control.Lens                 ((%=), (%~), (&), (.~), (<+=))
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
    (IRC.Ev.Channel _ _, Right msg) ->
      case LC.P.parseCommand msg of
        Left _err -> pure ()
        Right cmd -> do
          liftIO $ putStrLn $ "handling command: " <> show cmd
          execCommand cmd >>= replyTo source
    _ -> pure ()
  )


replyTo :: IRC.Ev.Source Text -> Maybe Text -> IRC.C.IRC s ()
replyTo source = maybe (pure ()) (IRC.C.replyTo source)


execCommand :: LC.Cmd.CoucouCmd -> IRC.C.IRC LC.St.CoucouState (Maybe Text)
execCommand = \case
  LC.Cmd.Nop -> pure Nothing
  LC.Cmd.Url mbTarget -> LC.Url.fetchUrlCommandHandler mbTarget
  LC.Cmd.Crypto coin target -> LC.C.cryptoCommandHandler coin target
  LC.Cmd.Date target -> LC.Date.dateCommandHandler target
  LC.Cmd.Cancer cancer target -> LC.Cancer.cancerCommandHandler cancer target

addTarget :: Maybe Text -> Text -> Text
addTarget mbTarget msg = case mbTarget of
  Nothing -> msg
  Just x  -> x <> ": " <> msg

updateLastUrlHandler :: IRC.Ev.EventHandler LC.St.CoucouState
updateLastUrlHandler = IRC.Ev.EventHandler
  (IRC.Ev.matchType IRC.Ev._Privmsg)
  (\source (_target, raw) -> case (source, raw) of
    (IRC.Ev.Channel _ _, Right msg) -> do
      let lastUrl = LC.P.parseUrl msg
      field @"csLastUrl" %= (lastUrl <|>)
    _ -> pure ()
  )
