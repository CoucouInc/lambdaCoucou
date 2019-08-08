module LambdaCoucou.Command where

import           Data.Text           (Text)
import           LambdaCoucou.Crypto (CryptoCoin)
import           LambdaCoucou.Cancer (CancerType)
import           LambdaCoucou.Help   (HelpCommand)

type Target = Maybe Text

data CoucouCmd
  = Url Target
  | Crypto (Either Text CryptoCoin) Target
  | Date Target
  | Cancer CancerType Target
  | ShoutCoucou
  | Help HelpCommand Target
  | PR Target
  | Joke Target
  | Nop
  deriving (Show, Eq)
