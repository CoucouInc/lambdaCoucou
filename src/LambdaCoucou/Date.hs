module LambdaCoucou.Date where

import qualified LambdaCoucou.HandlerUtils as LC.Hdl
import qualified LambdaCoucou.RepublicanCalendar as LC.RC
import qualified LambdaCoucou.State as LC.St
import qualified Network.IRC.Client as IRC.C
import qualified Network.IRC.Client.Events as IRC.Ev
import qualified Network.IRC.Client.Utils as IRC.Utils
import RIO
import qualified RIO.Text as T
import qualified RIO.Time as Time

dateCommandHandler ::
  Maybe Text ->
  IRC.C.IRC LC.St.CoucouState (Maybe Text)
dateCommandHandler target = do
  currentTime <- liftIO Time.getCurrentTime
  let (y, m, d) = Time.toGregorian (Time.utctDay currentTime)
  let mbRepublicanDate = LC.RC.gregorian2republican (fromIntegral y, m, d)
  let timeMsg =
        maybe
          "Oops, something went wrong when computing the date"
          LC.RC.prettyDate
          mbRepublicanDate
  pure $ Just $ LC.Hdl.addTarget target timeMsg

ctcpTimeHandler :: IRC.Ev.EventHandler s
ctcpTimeHandler =
  IRC.Ev.EventHandler
    (IRC.Ev.matchCTCP "TIME")
    ( \source _args -> case source of
        IRC.Ev.User usr -> do
          currentTime <- liftIO Time.getCurrentTime
          let (y, m, d) = Time.toGregorian (Time.utctDay currentTime)
          let mbRepublicanDate = LC.RC.gregorian2republican (fromIntegral y, m, d)
          let ctcpTime =
                T.pack (Time.formatTime Time.defaultTimeLocale "%H:%M.%S UTC" currentTime)
                  <> " - "
                  <> maybe "Oops, something went wrong when computing the date" LC.RC.showR mbRepublicanDate
          let ctcp = IRC.Utils.ctcpReply usr "TIME" [ctcpTime]
          IRC.C.send ctcp
        _ -> pure ()
    )
