module LambdaCoucou.Command where

import Data.Text (Text)

data CoucouCmd
  = Url (Maybe Text)
  | Nop
  deriving (Show, Eq)
