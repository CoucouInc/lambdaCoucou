{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module LambdaCoucou.Factoids
    ( getFactoid
    , getFactoids
    , randomFactoid
    , searchFactoids
    , adjustCounterFactoid
    , setFactoid
    , resetFactoid
    , deleteFactoid
    , augmentFactoid
    ) where

import Prelude hiding (null)
import Data.Monoid ((<>))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomR)
import Text.Read (readMaybe)
import Data.List (nub)
import Data.Text (Text, pack, unpack, isInfixOf, toLower, null, intercalate)
import qualified Data.Vector as V
import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as Map

import qualified Network.IRC.Client as IRC

import qualified LambdaCoucou.Types as T
import LambdaCoucou.Db (updateFactoids)
import LambdaCoucou.Dist (textDist)


getFactoid :: Text -> IRC.StatefulIRC T.BotState (Maybe Text)
getFactoid name = do
    factoidsT <- T._factoids <$> IRC.state
    factoids  <- liftIO $ STM.readTVarIO factoidsT
    case Map.lookup name factoids of
        Nothing   -> getSimilarFactoids name
        Just fact -> case fact of
            T.Counter n ->
                let payload = name <> ": " <> pack (show n)
                in  return (Just payload)
            T.Facts facts -> getRandomFactoid facts

getFactoids :: Text -> IRC.StatefulIRC T.BotState [Text]
getFactoids name = do
    factoidsT <- T._factoids <$> IRC.state
    factoids  <- liftIO $ STM.readTVarIO factoidsT
    return $ case Map.lookup name factoids of
        Nothing                -> ["Pas de factoids pour: " <> name]
        Just (T.Counter n    ) -> ["Counter: " <> pack (show n)]
        Just (T.Facts   facts) -> trimResults (V.toList facts)

getSimilarFactoids :: Text -> IRC.StatefulIRC T.BotState (Maybe Text)
getSimilarFactoids name = do
    factoidsT <- T._factoids <$> IRC.state
    factoids  <- liftIO $ STM.readTVarIO factoidsT
    let closeFactoids = filter (\f -> textDist name f <= 2) (Map.keys factoids)
    let l             = length closeFactoids
    if
        | l == 0
        -> return Nothing
        | l == 1
        -> return $ Just $ "did you mean " <> head closeFactoids
        | otherwise
        -> return
            $  Just
            $  "did you mean one of "
            <> prettyTruncate closeFactoids
            <> "?"
  where
    prettyTruncate :: [Text] -> Text
    prettyTruncate list =
        let list' = if length list < 5 then list else take 5 list ++ ["…"]
        in  intercalate ", " list'

searchFactoids :: Text -> Maybe Text -> IRC.StatefulIRC T.BotState [Text]
searchFactoids search mbSearch = do
    factoidsT <- T._factoids <$> IRC.state
    factoids  <- liftIO $ STM.readTVarIO factoidsT
    let search' = toLower search
    let flat    = flattenFactoids factoids
    let matchingPairs =
            nub $ filter (not . null . fst) $ filter (matchSearch search') flat
    let refined = case mbSearch of
            Nothing            -> matchingPairs
            Just refinedSearch -> filter
                (\(_, v) -> toLower refinedSearch `isInfixOf` toLower v)
                matchingPairs
    liftIO $ putStrLn $ "matching pairs: " <> show matchingPairs
    liftIO $ putStrLn $ "refined pairs: " <> show refined
    let withPrefix = fmap (\(k, v) -> k <> " -> " <> v) refined
    return $ trimResults withPrefix
  where
    matchSearch s (k, v) =
        (s `isInfixOf` toLower k) || (s `isInfixOf` toLower v)

getRandomFactoid :: V.Vector Text -> IRC.StatefulIRC T.BotState (Maybe Text)
getRandomFactoid facts = (fmap . fmap) (\i -> facts V.! i) (getRandomIdx facts)

randomFactoid :: IRC.StatefulIRC T.BotState (Maybe Text)
randomFactoid = do
    state    <- IRC.state
    factoids <- liftIO $ STM.readTVarIO (T._factoids state)
    let list = flattenFactoids factoids
    idx <- getRandomIdx list
    return $ fmap ((\(k, v) -> ">" <> k <> ": " <> v) . (\i -> list !! i)) idx

trimResults :: [Text] -> [Text]
trimResults xs | length xs <= 4 = xs
               | otherwise      = take 4 xs <> ["…"]

flattenFactoids :: T.Factoids -> [(Text, Text)]
flattenFactoids factoids = do
    (k, factoid) <- Map.toList factoids
    case factoid of
        T.Counter _     -> []
        T.Facts   facts -> do
            f <- V.toList facts
            return (k, f)

getRandomIdx :: (Foldable t) => t a -> IRC.StatefulIRC T.BotState (Maybe Int)
getRandomIdx xs = do
    let l = length xs
    if l == 0
        then return Nothing
        else do
            genT <- T._stdGen <$> IRC.state
            idx  <- liftIO $ STM.atomically $ do
                gen <- STM.readTVar genT
                let (a, gen') = randomR (0, l - 1) gen
                STM.writeTVar genT gen'
                return a
            return $ Just idx

adjustCounterFactoid
    :: (Int -> Int) -> IRC.UnicodeEvent -> Text -> IRC.StatefulIRC T.BotState ()
adjustCounterFactoid op ev name = do
    factoidsT <- T._factoids <$> IRC.state
    queue     <- T._writerQueue <$> IRC.state
    factoids  <- liftIO $ STM.atomically $ do
        factoids <- STM.readTVar factoidsT
        let factoids' = Map.adjust (incdecCounter op) name factoids
        STM.writeTVar factoidsT factoids'
        if factoids == factoids'
            then return Nothing
            else do
                updateFactoids queue factoids'
                return $ Just factoids'
    forM_ factoids (const $ IRC.reply ev name) -- sendFactoid target name)

incdecCounter :: (Int -> Int) -> T.Factoid -> T.Factoid
incdecCounter op (T.Counter n) = T.Counter $! op n
incdecCounter _  f             = f

setFactoid :: IRC.UnicodeEvent -> Text -> Text -> IRC.StatefulIRC T.BotState ()
setFactoid ev name val = do
    state <- IRC.state
    let factoidsT = T._factoids state
    factoids <- liftIO $ STM.readTVarIO factoidsT
    case Map.lookup name factoids of
        Just (T.Counter _) ->
            IRC.reply ev "Nope ! (There is already a factoid for that.)"
        Just (T.Facts facts) -> do
            let msg = if length facts > 1
                    then "Nope ! (There are existing factoids for that.)"
                    else "Nope ! (There is already a factoid for that.)"
            IRC.reply ev msg
        Nothing -> do
            let mbCounter = readMaybe (unpack val)
            let fact = maybe (T.Facts (V.singleton val)) T.Counter mbCounter
            liftIO $ STM.atomically $ do
                newFactoids <- STM.readTVar factoidsT
                let factoids' = Map.insert name fact newFactoids
                STM.writeTVar  factoidsT              factoids'
                updateFactoids (T._writerQueue state) factoids'
            IRC.reply ev "C'est noté !"

deleteFactoid :: IRC.UnicodeEvent -> Text -> IRC.StatefulIRC T.BotState ()
deleteFactoid ev name = do
    factoidsT <- T._factoids <$> IRC.state
    writerQ   <- T._writerQueue <$> IRC.state
    liftIO . STM.atomically $ do
        STM.modifyTVar' factoidsT (Map.delete name)
        factoids' <- STM.readTVar factoidsT
        updateFactoids writerQ factoids'
    IRC.reply ev "C'est effacé !"

-- not quite thread safe, but hell with it
resetFactoid
    :: IRC.UnicodeEvent -> Text -> Text -> IRC.StatefulIRC T.BotState ()
resetFactoid ev name val = do
    factoidsT <- T._factoids <$> IRC.state
    liftIO . STM.atomically $ STM.modifyTVar' factoidsT (Map.delete name)
    setFactoid ev name val

augmentFactoid
    :: IRC.UnicodeEvent -> Text -> Text -> IRC.StatefulIRC T.BotState ()
augmentFactoid ev name val = do
    factoidsT <- T._factoids <$> IRC.state
    writerQ   <- T._writerQueue <$> IRC.state
    msg       <- liftIO . STM.atomically $ do
        factoids <- STM.readTVar factoidsT
        case Map.lookup name factoids of
            Nothing ->
                return
                    $  "No factoid there yet, use `"
                    <> name
                    <> " = "
                    <> val
                    <> "`"
            Just fact -> do
                let factList = case fact of
                        T.Facts   fs -> fs
                        T.Counter n  -> V.singleton (pack $ show n)
                let fact'     = T.Facts $ V.snoc factList val
                let factoids' = Map.insert name fact' factoids
                STM.writeTVar  factoidsT factoids'
                updateFactoids writerQ   factoids'
                return "C'est noté"
    IRC.reply ev msg
