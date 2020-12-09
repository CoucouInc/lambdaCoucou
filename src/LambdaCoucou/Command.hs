module LambdaCoucou.Command where

import LambdaCoucou.Cancer (CancerType)
import LambdaCoucou.Crypto (CryptoCoin)
import LambdaCoucou.Help (HelpCommand)
import LambdaCoucou.Remind (RemindCmd)
import LambdaCoucou.UserSettings (SettingsCmd)
import RIO

type Target = Maybe Text

data CoucouCmd
  = Url Int Target
  | Crypto (Either Text CryptoCoin) Target
  | Date Target
  | Cancer CancerType Target
  | ShoutCoucou
  | HeyCoucou
  | Help HelpCommand Target
  | PR Target
  | Joke Target
  | Remind RemindCmd
  | Settings SettingsCmd
  | YTSearch [Text] Target
  | Sed Text Text
  -- | (raw regex) (replacement text)
  | LiveStreams Target
  | StupidCase [Text] Target
  | Nop
  deriving (Show, Eq)
