module LambdaCoucou.Help where

import qualified LambdaCoucou.HandlerUtils as LC.Hdl
import qualified LambdaCoucou.State as LC.St
import qualified Network.IRC.Client as IRC.C
import RIO

data HelpCommand
  = Url
  | Crypto
  | Date
  | Cancer
  | ShoutCoucou
  | General
  | Joke
  | Unknown Text
  deriving (Show, Eq)

helpCommandHandler ::
  HelpCommand ->
  Maybe Text ->
  IRC.C.IRC LC.St.CoucouState (Maybe Text)
helpCommandHandler hlpCmd target = do
  let generalMsg = "`&help cmd` with cmd one of [url, crypto, date, cancer, coucou, joke]"
  let msg = case hlpCmd of
        Url ->
          "Grab the title of the last url seen in the chan."
        Crypto ->
          "Get the current exchange rate for the given crypto coin."
        Date ->
          "Give today's date according to the French Republican calendar."
        Cancer ->
          "`&cancer [match]`. Return the matching cancerous link, or a random one if no argument given. The list is at: https://github.com/CoucouInc/lalalaliste/blob/master/cancer.txt"
        ShoutCoucou ->
          "Say 'coucou X' where X is a random member/lurker of the chan."
        General ->
          generalMsg
        Joke ->
          "Gives a random (bad) joke."
        Unknown cmd ->
          "Unknown command: " <> cmd <> ". " <> generalMsg
  pure $ Just $ LC.Hdl.addTarget target msg
