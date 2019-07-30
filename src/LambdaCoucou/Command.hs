module LambdaCoucou.Command where

import           Data.Text           (Text)
import           LambdaCoucou.Crypto (CryptoCoin)
import           LambdaCoucou.Cancer (CancerType)
import           LambdaCoucou.Help   (HelpCommand)

data CoucouCmd
  = Url (Maybe Text)
  | Crypto (Either Text CryptoCoin) (Maybe Text)
  | Date (Maybe Text)
  | Cancer CancerType (Maybe Text)
  | ShoutCoucou
  | Help HelpCommand
  | Nop
  deriving (Show, Eq)
