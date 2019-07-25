module LambdaCoucou.Command where

import           Data.Text           (Text)
import           LambdaCoucou.Crypto (CryptoCoin)
import           LambdaCoucou.Cancer (CancerType)

data CoucouCmd
  = Url (Maybe Text)
  | Crypto (Either Text CryptoCoin) (Maybe Text)
  | Date (Maybe Text)
  | Cancer CancerType (Maybe Text)
  | Nop
  deriving (Show, Eq)
