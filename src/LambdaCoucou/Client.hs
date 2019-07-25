{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Client where

import qualified Network.IRC.Client              as IRC.C
import qualified Network.IRC.Client.Events       as IRC.Ev
import qualified Network.IRC.Client.Lens         as IRC.L
import qualified Network.IRC.Client.Utils        as IRC.Utils
import qualified Network.IRC.CTCP                as CTCP

import           Control.Applicative
import           Control.Lens                    ((^.), (%=), (%~), (&), (.~), (<+=))
import           Control.Lens.Extras             (is)
import           Control.Monad
import           Control.Monad.IO.Class          (liftIO)
import qualified Control.Monad.State.Strict      as St
import qualified Data.DateTime                   as DT
import           Data.Generics.Product.Fields    (field)
import qualified Data.Maybe                      as Mb
import qualified Data.Text                       as Tx
import qualified Data.Text.IO                    as Tx.IO
import qualified Data.Time.Format                as T.F
import qualified System.Environment              as Env
import qualified GHC.Conc                        as Conc

import qualified LambdaCoucou.Command            as LC.Cmd
import qualified LambdaCoucou.Parser             as LC.P
import qualified LambdaCoucou.RepublicanCalendar as LC.RC
import qualified LambdaCoucou.State              as LC.St
import qualified LambdaCoucou.Url                as LC.Url


test :: IO ()
test = do
  let tlsConfig = IRC.C.WithDefaultConfig "chat.freenode.net" 6697
  let connectionConfig = IRC.C.tlsConnection tlsConfig
        & IRC.L.logfunc .~ IRC.C.stdoutLogger
  let instanceConfig = IRC.C.defaultInstanceConfig "lambdatest"
        & IRC.L.channels .~ ["#gougoutest"]
        & IRC.L.version .~ "lambdacoucou-v2"
        & IRC.L.handlers %~ ([updateLastUrlHandler, commandHandler, ctcpTimeHandler, testEventHandler] ++)
  ytApiKey <- LC.St.YoutubeAPIKey . Tx.pack <$> Env.getEnv "YT_API_KEY"
  IRC.C.runClient connectionConfig instanceConfig (LC.St.initialState ytApiKey)
  putStrLn "done"

-- test :: IO ()
-- test = do
--   putStrLn "----------------- starting"
--   -- let testUrl = "https://geekingfrog.com/nope"
--   let testUrl = "https://www.youtube.com/watch?v=dEC_HiVtpRU"
--   ytApiKey <- LC.St.YoutubeAPIKey . Tx.pack <$> Env.getEnv "YT_API_KEY"
--   LC.Url.fetchUrlData ytApiKey testUrl >>= \x ->
--     Tx.IO.putStrLn $ either (LC.Url.showFetchError testUrl) id x

testEventHandler :: IRC.Ev.EventHandler LC.St.CoucouState
testEventHandler = IRC.Ev.EventHandler
  (IRC.Ev.matchType IRC.Ev._Privmsg)
  (\source x@(target, raw) -> case (source, raw) of
    (IRC.Ev.Channel _ _, Right msg) -> do
      liftIO $ putStrLn "testevent handler"
      let lastUrl = LC.P.parseUrl msg
      field @"csLastUrl" %= (\old -> lastUrl <|> old)
      newCounter <- field @"csCounter" <+= 1
      st <- St.get
      let resp = "state: " <> show st
      liftIO $ putStrLn $ "from: " <> show source <> " - " <> resp
      IRC.C.replyTo source (Tx.pack resp)
    _ -> pure ()
  )

commandHandler :: IRC.Ev.EventHandler LC.St.CoucouState
commandHandler = IRC.Ev.EventHandler
  (IRC.Ev.matchType IRC.Ev._Privmsg)
  (\source (target, raw) -> case (source, raw) of
    (IRC.Ev.Channel _ _, Right msg) ->
      case LC.P.parseCommand msg of
        Left _err -> pure ()
        Right cmd -> do
          liftIO $ putStrLn $ "handling command: " <> show cmd
          execCommand cmd >>= reply source
    _ -> pure ()
  )


reply :: IRC.Ev.Source Tx.Text -> Maybe Tx.Text -> IRC.C.IRC s ()
reply source = maybe (pure ()) (IRC.C.replyTo source)


execCommand :: LC.Cmd.CoucouCmd -> IRC.C.IRC LC.St.CoucouState (Maybe Tx.Text)
execCommand = \case
  LC.Cmd.Nop -> pure Nothing
  LC.Cmd.Url _mbTarget -> do
    st <- St.get
    case LC.St.csLastUrl st of
      Nothing -> pure Nothing
      Just url ->
        LC.Url.fetchUrlData (LC.St.csYtAPIKey st) url >>= \case
          Left err -> pure $ Just $ LC.Url.showFetchError url err
          Right title -> pure $ Just $ title <> " [" <> url <> "]"

updateLastUrlHandler :: IRC.Ev.EventHandler LC.St.CoucouState
updateLastUrlHandler = IRC.Ev.EventHandler
  (IRC.Ev.matchType IRC.Ev._Privmsg)
  (\source x@(target, raw) -> case (source, raw) of
    (IRC.Ev.Channel _ _, Right msg) -> do
      let lastUrl = LC.P.parseUrl msg
      field @"csLastUrl" %= (lastUrl <|>)
    _ -> pure ()
  )


ctcpTimeHandler :: IRC.Ev.EventHandler s
ctcpTimeHandler = IRC.Ev.EventHandler
  (IRC.Ev.matchCTCP "TIME")
  (\source _args -> case source of
    IRC.Ev.User usr -> do
      currentTime <- liftIO DT.getCurrentTime
      let (y, m, d) = DT.toGregorian' currentTime
      let mbRepublicanDate = LC.RC.gregorian2republican (fromIntegral y, m, d)
      let ctcpTime
            = Tx.pack (T.F.formatTime T.F.defaultTimeLocale "%H:%M.%S UTC" currentTime)
            <> " - "
            <> Mb.maybe "Oops, something went wrong when computing the date" LC.RC.showR mbRepublicanDate
      let ctcp = IRC.Utils.ctcpReply usr "TIME" [ctcpTime]
      IRC.C.send ctcp
    _ -> pure ()
  )
