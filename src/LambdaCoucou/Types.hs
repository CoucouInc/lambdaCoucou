{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module LambdaCoucou.Types where

import System.Random (StdGen)
import Data.ByteString (ByteString)
import Data.Text (Text)
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
jsonToInt = withScientific "int" $ \n -> case Sci.toBoundedInteger n of
    Nothing -> error ("not valid integer: " ++ show n)
    Just i  -> return i


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
