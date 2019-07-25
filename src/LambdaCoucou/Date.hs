{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Date where

import           Control.Monad.IO.Class          (liftIO)
import qualified Data.DateTime                   as DT
import           Data.Text                       (Text)
import qualified Network.IRC.Client              as IRC.C
import qualified Data.Maybe                      as Mb
import qualified Network.IRC.Client.Events       as IRC.Ev
import qualified Data.Text                       as Tx
import qualified Network.IRC.Client.Utils        as IRC.Utils

import qualified LambdaCoucou.HandlerUtils       as LC.Hdl
import qualified LambdaCoucou.RepublicanCalendar as LC.RC
import qualified LambdaCoucou.State              as LC.St
import qualified Data.Time.Format                as T.F

dateCommandHandler
  :: Maybe Text
  -> IRC.C.IRC LC.St.CoucouState (Maybe Text)

dateCommandHandler target = do
  currentTime <- liftIO DT.getCurrentTime
  let (y, m, d) = DT.toGregorian' currentTime
  let mbRepublicanDate = LC.RC.gregorian2republican (fromIntegral y, m, d)
  let timeMsg = Mb.maybe
        "Oops, something went wrong when computing the date"
        LC.RC.prettyDate
        mbRepublicanDate

  pure $ Just $ LC.Hdl.addTarget target timeMsg

-- Nous sommes aujourd'hui le 7 Thermidor 227 − jour de l'armoise − et c'est un Septidi.

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
