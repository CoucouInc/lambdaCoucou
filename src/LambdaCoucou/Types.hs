module LambdaCoucou.Types where

import System.Random (StdGen)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import GHC.Conc.Sync (TVar)
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Data.Aeson
import qualified Data.Scientific as Sci

import qualified Network.IRC.Client as IRC

type Factoids = HashMap Text Factoid

data Factoid
    = Facts (Vector Text)
    | Counter !Int
    deriving (Show, Eq)

instance FromJSON Factoid where
    parseJSON (Array facts) = Facts <$> mapM parseJSON facts
    parseJSON (Number n) = case Sci.toBoundedInteger n of
        Nothing -> mempty
        Just i -> return $ Counter i
    parseJSON _ = mempty



data BotState = BotState
    { _stdGen :: TVar StdGen
    , _factoids :: TVar Factoids
    }

data CoucouCmd
    = CoucouCmdNop
      -- | CoucouCmdCoucou (Maybe Text) -- Maybe nickname
    | CoucouCmdCancer !(Maybe Text) -- Maybe substring
    | CoucouCmdFactoid !Text CmdFactoidType
     deriving (Eq)

data CmdFactoidType
    = GetFactoid
    | SetFactoid Text
    | ResetFactoid Text
    | AugmentFactoid Text
    | DeleteFactoid
    | IncFactoid
    | DecFactoid
     deriving (Eq, Show)

instance Show CoucouCmd where
    show CoucouCmdNop = "no op"
    -- show (CoucouCmdCoucou _) = "Counts of coucou"
    show (CoucouCmdCancer search) =
        case search of
            Nothing -> "A random cancer"
            Just s -> "The first cancer matching " <> unpack s
    show (CoucouCmdFactoid name factoidType) =
        case factoidType of
            GetFactoid -> "Get the factoid with name: " <> unpack name
            SetFactoid val ->
                "Set a new factoid with name: " <> unpack name <> " and val: " <>
                unpack val
            ResetFactoid val ->
                "Reset factoid: " <> unpack name <> " to val: " <> unpack val
            AugmentFactoid val ->
                "Augment factoid: " <> unpack name <> " with val: " <> unpack val
            DeleteFactoid -> "Delete factoid: " <> unpack name
