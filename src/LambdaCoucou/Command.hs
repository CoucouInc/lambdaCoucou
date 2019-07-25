module LambdaCoucou.Command where

import           Data.Text           (Text)
import           LambdaCoucou.Crypto (CryptoCoin)

data CoucouCmd
  = Url (Maybe Text)
  | Crypto (Either Text CryptoCoin) (Maybe Text)
  | Date (Maybe Text)
  | Nop
  deriving (Show, Eq)
