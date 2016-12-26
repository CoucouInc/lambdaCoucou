module LambdaCoucou.Types where

import System.Random (StdGen)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import GHC.Conc.Sync (TVar)
import qualified Network.IRC.Client as IRC


data BotState = BotState
    { _stuff :: Text
    , _stdGen :: TVar StdGen
    }

data CoucouCmd
    = CoucouCmdNop
    | CoucouCmdCoucou (Maybe Text) -- Maybe nickname
    | CoucouCmdCancer (Maybe Text) -- Maybe substring
    deriving (Eq)

instance Show CoucouCmd where
    show CoucouCmdNop = "no op"
    show (CoucouCmdCoucou _) = "Counts of coucou"
    show (CoucouCmdCancer search) = case search of
        Nothing -> "A random cancer"
        Just s -> "The first cancer matching " <> unpack s
