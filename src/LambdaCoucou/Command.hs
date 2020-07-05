module LambdaCoucou.Command where

import LambdaCoucou.Cancer (CancerType)
import LambdaCoucou.Crypto (CryptoCoin)
import LambdaCoucou.Help (HelpCommand)
import LambdaCoucou.Remind (RemindCmd)
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
  | Nop
  deriving (Show, Eq)
