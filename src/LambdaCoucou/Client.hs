{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Client where

import qualified Network.IRC.Client as IRC.C
import qualified Network.IRC.Client.Lens as IRC.L
import qualified Network.IRC.Client.Events as IRC.Ev
import qualified Network.IRC.Client.Utils as IRC.Utils
import qualified Network.IRC.CTCP as CTCP

import Control.Lens ((.~), (&), (%~))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Tx
import qualified Data.Text.IO as Tx.IO
import qualified Data.DateTime as DT
import qualified Data.Time.Format as T.F
import qualified Data.Maybe as Mb

import qualified LambdaCoucou.RepublicanCalendar as RC

test :: IO ()
test = do
  let tlsConfig = IRC.C.WithDefaultConfig "chat.freenode.net" 6697
  let connectionConfig = IRC.C.tlsConnection tlsConfig
        & IRC.L.logfunc .~ IRC.C.stdoutLogger
  let instanceConfig = IRC.C.defaultInstanceConfig "lambdatest"
        & IRC.L.channels .~ ["#gougoutest"]
        & IRC.L.version .~ "lambdacoucou-v2"
        & IRC.L.ignore .~ [("coucoubot", Nothing)]
        & IRC.L.handlers %~ ([ctcpTimeHandler, testEventHandler] ++ )
  IRC.C.runClient connectionConfig instanceConfig ()
  putStrLn "done"

data CoucouState
  = CoucouState
      { _csLastUrl :: Maybe Tx.Text
      , shutuphlint :: Int
      }
  deriving (Show)

testEventHandler :: IRC.Ev.EventHandler s
testEventHandler = IRC.Ev.EventHandler
  (IRC.Ev.matchType IRC.Ev._Privmsg)
  (\source x@(target, msg) -> do
    liftIO $ print x
  )


ctcpTimeHandler :: IRC.Ev.EventHandler s
ctcpTimeHandler = IRC.Ev.EventHandler
  (IRC.Ev.matchCTCP "TIME")
  (\source _args -> case source of
    IRC.Ev.User usr -> do
      currentTime <- liftIO DT.getCurrentTime
  -- TIME Wed Jul 24 11:53:17 UTC 2019
      let (y, m, d) = DT.toGregorian' currentTime
      let mbRepublicanDate = RC.gregorian2republican (fromIntegral y, m, d)
      let ctcpTime
            = Tx.pack (T.F.formatTime T.F.defaultTimeLocale "%H:%M.%S UTC" currentTime)
            <> " - "
            <> Mb.maybe "Oops, something went wrong when computing the date" RC.showR mbRepublicanDate
      let ctcp = IRC.Utils.ctcpReply usr "TIME" [ctcpTime]
      IRC.C.send ctcp
    _ -> pure ()
  )
