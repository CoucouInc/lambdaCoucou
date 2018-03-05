{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Control.Monad.IO.Class
import Network.HTTP.Req
import Control.Exception (throwIO)

-- for CLI args
data Opts = Opts
    { optHost :: !ByteString
    , optPort :: !Int
    , optNick :: !Text
    , optChan :: !Text
    } deriving (Show)

-- for http requests
newtype HttpM m a = HttpM { runHttp :: m a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance (MonadIO m) => MonadHttp (HttpM m) where
    handleHttpException = liftIO . throwIO


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
    , _reminders :: Vector Remind
    } deriving (Show)

instance ToJSON SocialRecord where
    toJSON r =
        object
            [ "coucous" .= _coucous r
            , "lastSeen" .= toJSON (_lastSeen r)
            , "toTell" .= toJSON (_toTell r)
            , "reminders" .= toJSON (_reminders r)
            ]

instance FromJSON SocialRecord where
    parseJSON =
        withObject "social record" $
        \o -> do
            coucous <- o .: "coucous" >>= jsonToInt
            lastSeen <- o .: "lastSeen"
            messages <- o .:? "toTell" .!= empty
            reminders <- o .:? "reminders" .!= empty
            return
                SocialRecord
                { _coucous = coucous
                , _lastSeen = lastSeen
                , _toTell = messages
                , _reminders = reminders
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
    , _toTellTs :: !Timestamp
    , _toTellFrom :: !Text
    , _toTellOnChannel :: !Text
    } deriving (Show)

instance ToJSON ToTell where
    toJSON t =
        object
            [ "msg" .= _toTellMsg t
            , "ts" .= _toTellTs t
            , "from" .= _toTellFrom t
            , "onChannel" .= _toTellOnChannel t
            ]

instance FromJSON ToTell where
    parseJSON =
        withObject "to tell object" $ parseTell ToTell

data Remind = Remind
    { _remindMsg :: !Text
    , _remindTs :: !Timestamp
    , _remindFrom :: !Text
    , _remindOnChannel :: !Text
    } deriving (Show)

instance ToJSON Remind where
    toJSON r =
        object
            [ "msg" .= _remindMsg r
            , "ts" .= _remindTs r
            , "from" .= _remindFrom r
            , "onChannel" .= _remindOnChannel r
            ]

instance FromJSON Remind where
    parseJSON =
        withObject "remind object" $ parseTell Remind

parseTell :: (Text -> Timestamp -> Text -> Text -> t) -> Object -> Parser t
parseTell ctor o = do
    msg <- o .: "msg"
    ts <- o .: "ts"
    from <- o .: "from"
    chan <- o .: "onChannel"
    return $ ctor msg ts from chan

type WriterQueue = TBQueue (Either Factoids SocialRecords)

data BotState = BotState
    { _stdGen :: TVar StdGen
    , _factoids :: TVar Factoids
    , _socialDb :: TVar SocialRecords
    , _writerQueue :: WriterQueue
    , _version :: !(String, String)
    , _lastUrl :: TVar (Maybe Text)
    }

data CoucouCmd
    = CoucouCmdNop
    | CoucouCmdCancer !(Maybe Text)
                      !(Maybe Text) -- Maybe search substr, Maybe nick to hl
    | CoucouCmdFactoid !Text
                       CmdFactoidType
    | CoucouCmdRandomFactoid
    | CoucouCmdGetCoucou !(Maybe Text) -- Maybe nick to hl
    | CoucouCmdIncCoucou
    | CoucouCmdCoucouRank
    | CoucouCmdLastSeen !Text
                        !(Maybe Text) -- Maybe nick to hl
    | CoucouCmdVersion
    | CoucouCmdTell !Text !Text (Maybe Timestamp) -- nick payload maybe(delay)
    | CoucouCmdRemind !Text !Text (Maybe Timestamp) -- nick payload maybe(delay)
    | CoucouCmdHelp (Maybe CoucouHelpType)
    | CoucouCmdUrl
    | CoucouCmdCryptoRate !Text !(Maybe Text) -- symbol of the crypto to fetch, maybe nick to hl
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
    | SearchFactoids !(Maybe Text) -- maybe search term for values
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
               SearchFactoids mbSearch ->
                   "Search factoids with content: " <> name' <> " and refine search with " <> show mbSearch <>
                   "."
    show CoucouCmdRandomFactoid = "Get a random factoid"
    show (CoucouCmdGetCoucou (Just nick)) = "Show count of coucou for " <> unpack nick
    show (CoucouCmdGetCoucou Nothing) = "Show count of coucou for the sender of the message"
    show CoucouCmdIncCoucou = "Increment coucou count for sender of this message"
    show CoucouCmdCoucouRank = "Who has the most coucou"
    show (CoucouCmdLastSeen nick mbHl) =
        "How long since " <> unpack nick <> " has been seen ? (for user " <> show mbHl <> ")."
    show CoucouCmdVersion = "Send git info of the bot"
    show (CoucouCmdTell nick payload mbDelay) =
        "Tell " <> unpack nick <> ": " <> unpack payload <> " after " <> show mbDelay <> "s."
    show (CoucouCmdRemind nick payload mbDelay) =
        "Send " <> unpack nick <> ": " <> unpack payload <> " after " <> show mbDelay <> "s."
    show CoucouCmdUrl = "Grab infos from last url"
    show (CoucouCmdHelp Nothing) = "Help, list available commands."
    show (CoucouCmdHelp (Just t)) = "Help for the command " <> show t <> "."
    show (CoucouCmdCryptoRate sym _) = "Current rate for " <> unpack sym <> " in usd"

data CoucouHelpType
    = TypeCancer
    | TypeFactoid
    | TypeCoucou
    | TypeCoucouRank
    | TypeSeen
    | TypeTell
    | TypeRemind
    | TypeVersion
    | TypeRandom
    | TypeUrl
    | TypeUnknown !Text
    deriving (Eq, Show)
