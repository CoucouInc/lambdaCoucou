{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Types where

import System.Random (StdGen)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TBQueue (TBQueue)
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Scientific as Sci

type Factoids = HashMap Text Factoid

data Factoid
    = Facts (Vector Text)
    | Counter !Int
    deriving (Show, Eq)

instance FromJSON Factoid where
    parseJSON (Array facts) = Facts <$> mapM parseJSON facts
    parseJSON (Number n) =
        case Sci.toBoundedInteger n of
            Nothing -> mempty
            Just i -> return $ Counter i
    parseJSON _ = mempty

instance ToJSON Factoid where
    toJSON (Facts v) = toJSON v
    toJSON (Counter n) = toJSON n

type SocialRecords = HashMap Text SocialRecord
type Timestamp = Integer

data SocialRecord = SocialRecord
    { _coucous :: !Int
    , _lastSeen :: !Timestamp
    } deriving (Show)

instance ToJSON SocialRecord where
    toJSON r = object ["coucous" .= _coucous r, "lastSeen" .= toJSON (_lastSeen r)]

instance FromJSON SocialRecord where
    parseJSON =
        withObject "social record" $
        \o -> do
            coucous <- o .: "coucous"
            lastSeen <- o .: "lastSeen"
            case coucous of
                (Number n) ->
                    case Sci.toBoundedInteger n of
                        Nothing -> error ("not a number for coucous: " ++ show n)
                        Just i -> return $ SocialRecord i lastSeen
                _ -> typeMismatch "Invalid value for social record" coucous

type WriterQueue = TBQueue (Either Factoids SocialRecords)

data BotState = BotState
    { _stdGen :: TVar StdGen
    , _factoids :: TVar Factoids
    , _socialDb :: TVar SocialRecords
    , _writerQueue :: WriterQueue
    }

data CoucouCmd
    = CoucouCmdNop
    | CoucouCmdCancer !(Maybe Text) -- Maybe substring
    | CoucouCmdFactoid !Text
                       CmdFactoidType
    | CoucouCmdGetCoucou !(Maybe Text) -- Maybe nick of user to query
    | CoucouCmdIncCoucou
    | CoucouCmdLastSeen !Text
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
            Nothing -> "A random cancer."
            Just s -> "The first cancer matching " <> unpack s <> "."
    show (CoucouCmdFactoid name factoidType) =
        let name' = unpack name
        in case factoidType of
               GetFactoid -> "Get the factoid with name: " <> name' <> "."
               SetFactoid val -> "Set a new factoid with name: " <> name' <> " and val: " <> unpack val <> "."
               ResetFactoid val -> "Reset factoid: " <> name' <> " to val: " <> unpack val <> "."
               AugmentFactoid val -> "Augment factoid: " <> name' <> " with val: " <> unpack val <> "."
               DeleteFactoid -> "Delete factoid: " <> name' <> "."
               IncFactoid -> "Increment counter: " <> name' <> "."
               DecFactoid -> "Decrement counter: " <> name' <> "."
    show (CoucouCmdGetCoucou (Just nick)) = "Show count of coucou for " <> unpack nick
    show (CoucouCmdGetCoucou Nothing) = "Show count of coucou for the sender of the message"
    show CoucouCmdIncCoucou = "Increment coucou count for sender of this message"
    show (CoucouCmdLastSeen nick) = "How long since " <> unpack nick <> " has been seen ?"
