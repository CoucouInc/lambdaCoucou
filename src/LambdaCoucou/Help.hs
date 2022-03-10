module LambdaCoucou.Help where

import qualified LambdaCoucou.HandlerUtils as LC.Hdl
import qualified LambdaCoucou.State as LC.St
import qualified Network.IRC.Client as IRC.C
import RIO

data HelpCommand
  -- = Url
  -- | Crypto
  -- | Date
  = Cancer
  | ShoutCoucou
  | General
  -- | Joke
  | Remind
  | Settings
  | YTSearch
  | LiveStreams
  | StupidCase
  | Unknown Text
  deriving (Show, Eq)

helpCommandHandler ::
  HelpCommand ->
  Maybe Text ->
  IRC.C.IRC LC.St.CoucouState (Maybe Text)
helpCommandHandler hlpCmd target = do
  let generalMsg = "`&help cmd` with cmd one of [cancer, coucou, remind, settings, ytSearch|yt_search, live, stupid|stupidcase]"
  let msg = case hlpCmd of
        -- Url ->
        --   "[λurl | λurl n] Grab the title of the last url seen in the chan. If n given > 0, gives the previous n url. λurl 1 is the second to last url."
        -- Crypto ->
        --   "Get the current exchange rate for the given crypto coin."
        -- Date ->
        --   "Give today's date according to the French Republican calendar."
        Cancer ->
          "`&cancer [match]`. Return the matching cancerous link, or a random one if no argument given. The list is at: https://github.com/CoucouInc/lalalaliste/blob/master/cancer.txt"
        ShoutCoucou ->
          "Say 'coucou X' where X is a random member/lurker of the chan."
        General ->
          generalMsg
        -- Joke ->
        --   "Gives a random (bad) joke."
        Remind ->
          "Reminder. &remind (in <duration>|at <time>|tomorrow (at <time>)). ex: &remind at 2020-06-28 12:34 coucou. &remind à 19:00 text. &remind in 1y 10M 1d 2h10m another coucou. &remind lundi coucou. &remind tuesday at 12 manger. The timezone depends on the user settings, defaults to UTC. See λhelp settings. &remind list. &remind del <ID>"
        Settings ->
          "Save some user prefference. &settings set/unset <setting>. Setting can be: tz: a timezone, for example Europe/Paris. `&settings show` to display current preferences"
        YTSearch ->
          "Returns the first youtube video result for the query. Ex: &ytSearch je suis charlie"
        LiveStreams ->
          "List all the twitch streams currently live for this channel."
        StupidCase ->
          "CoNvErT a StRiNg To StUpIdCaSe."
        Unknown cmd ->
          "Unknown command: " <> cmd <> ". " <> generalMsg
  pure $ Just $ LC.Hdl.addTarget target msg
