{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module LambdaCoucou.Debug where

import           Control.Lens               ((<+=), (^.))
import           Control.Monad
import           Control.Monad.IO.Class     (liftIO)
import qualified Control.Monad.State.Strict as St
import           Data.Generics.Product      (field)
import qualified Data.Text                  as Tx
import qualified LambdaCoucou.State         as LC.St
import qualified Network.IRC.Client         as IRC.C
import qualified Network.IRC.Client.Events  as IRC.Ev

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
