{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Types where

import System.Random (StdGen)
import Data.Monoid ((<>))
import Data.ByteString (ByteString)
import Data.Text (Text, unpack)
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TBQueue (TBQueue)
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector, empty)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Scientific as Sci

-- for CLI args
data Opts = Opts
    { optHost :: !ByteString
    , optPort :: !Int
    , optNick :: !Text
    , optChan :: !Text
    } deriving (Show)

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
    , _toTell :: Vector ToTell
    } deriving (Show)

instance ToJSON SocialRecord where
    toJSON r =
        object
            [ "coucous" .= _coucous r
            , "lastSeen" .= toJSON (_lastSeen r)
            , "toTell" .= toJSON (_toTell r)
            ]

instance FromJSON SocialRecord where
    parseJSON =
        withObject "social record" $
        \o -> do
            coucous <- o .: "coucous" >>= jsonToInt
            lastSeen <- o .: "lastSeen"
            messages <- o .:? "toTell" .!= empty
            return
                SocialRecord
                { _coucous = coucous
                , _lastSeen = lastSeen
                , _toTell = messages
                }

jsonToInt :: Value -> Parser Int
jsonToInt =
    withScientific "int" $
    \n ->
         case Sci.toBoundedInteger n of
             Nothing -> error ("not valid integer: " ++ show n)
             Just i -> return i


data ToTell = ToTell
    { _toTellMsg :: !Text
    , _toTellIsPrivate :: !Bool
    , _toTellTs :: !Timestamp
    } deriving (Show)

instance ToJSON ToTell where
    toJSON t =
        object ["msg" .= _toTellMsg t, "isPrivate" .= _toTellIsPrivate t, "ts" .= _toTellTs t]

instance FromJSON ToTell where
    parseJSON =
        withObject "social record" $
        \o -> do
            msg <- o .: "msg"
            isPrivate <- o .: "isPrivate"
            ts <- o .: "ts"
            return $ ToTell msg isPrivate ts


type WriterQueue = TBQueue (Either Factoids SocialRecords)

data BotState = BotState
    { _stdGen :: TVar StdGen
    , _factoids :: TVar Factoids
    , _socialDb :: TVar SocialRecords
    , _writerQueue :: WriterQueue
    , _version :: !(String, String)
    }

data CoucouCmd
    = CoucouCmdNop
    | CoucouCmdCancer !(Maybe Text)
                      !(Maybe Text) -- Maybe search substr, Maybe nick to hl
    | CoucouCmdFactoid !Text
                       CmdFactoidType
    | CoucouCmdGetCoucou !(Maybe Text) -- Maybe nick to hl
    | CoucouCmdIncCoucou
    | CoucouCmdLastSeen !Text
                        !(Maybe Text) -- Maybe nick to hl
    | CoucouCmdVersion
    | CoucouCmdTell !Text !Text (Maybe Timestamp) -- nick payload maybe(delay)
    deriving (Eq)

data CmdFactoidType
    = GetFactoid !(Maybe Text) -- maybe user to hl
    | SetFactoid Text
    | ResetFactoid Text
    | AugmentFactoid Text
    | DeleteFactoid
    | IncFactoid
    | DecFactoid
    | SeeFactoids
    | SearchFactoids
    deriving (Eq, Show)

instance Show CoucouCmd where
    show CoucouCmdNop = "no op"
    -- show (CoucouCmdCoucou _) = "Counts of coucou"
    show (CoucouCmdCancer search mbHl) =
        case search of
            Nothing -> "A random cancer for nick " <> show mbHl <> "."
            Just s -> "The first cancer matching " <> unpack s <> " for " <> show mbHl <> "."
    show (CoucouCmdFactoid name factoidType) =
        let name' = unpack name
        in case factoidType of
               GetFactoid mbHl ->
                   "Get the factoid with name: " <> name' <> " and hl: " <> show mbHl <> "."
               SetFactoid val ->
                   "Set a new factoid with name: " <> name' <> " and val: " <> unpack val <> "."
               ResetFactoid val -> "Reset factoid: " <> name' <> " to val: " <> unpack val <> "."
               AugmentFactoid val ->
                   "Augment factoid: " <> name' <> " with val: " <> unpack val <> "."
               DeleteFactoid -> "Delete factoid: " <> name' <> "."
               IncFactoid -> "Increment counter: " <> name' <> "."
               DecFactoid -> "Decrement counter: " <> name' <> "."
               SeeFactoids -> "List factoids for name: " <> name' <> "."
               SearchFactoids -> "Search factoids with content: " <> name' <> "."
    show (CoucouCmdGetCoucou (Just nick)) = "Show count of coucou for " <> unpack nick
    show (CoucouCmdGetCoucou Nothing) = "Show count of coucou for the sender of the message"
    show CoucouCmdIncCoucou = "Increment coucou count for sender of this message"
    show (CoucouCmdLastSeen nick mbHl) =
        "How long since " <> unpack nick <> " has been seen ? (for user " <> show mbHl <> ")."
    show CoucouCmdVersion = "Send git info of the bot"
    show (CoucouCmdTell nick payload mbDelay) =
        "Tell " <> unpack nick <> ": " <> unpack payload <> " after " <> show mbDelay <> "s."
