{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Cancer where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mempty, (<>))
import Data.Text (Text, toLower, isInfixOf, pack)
import Data.Text.Encoding (decodeUtf8')
import Network.HTTP.Req
import Text.Megaparsec
import Text.Megaparsec.Error (parseErrorPretty)
import System.Random (randomR)
import qualified GHC.Conc.Sync as Conc

import qualified Network.IRC.Client as IRC
import qualified LambdaCoucou.Types as T


parseCancer :: Text -> Either (ParseError Char Dec) [(Text, Text)]
parseCancer = parse cancerParser ""

cancerParser :: Parsec Dec Text [(Text, Text)]
cancerParser = many cancerLine

cancerLine :: Parsec Dec Text (Text, Text)
cancerLine = do
    title <- pack <$> many (noneOf [':'])
    string ": "
    cancerUrl <- pack <$> manyTill anyChar eol
    return (title, cancerUrl)



-- fetchCancer :: Maybe Text -> IO (Maybe Text)
fetchCancer :: Maybe Text -> IRC.StatefulIRC T.BotState (Maybe (Text, Text))
fetchCancer search = do
    parsed <- liftIO getCancer
    case parsed of
        Nothing -> return Nothing
        Just cancers ->
            case search of
                Nothing -> getRandomCancer cancers
                Just substr -> return $ getOneCancer substr cancers

getCancer :: IO (Maybe [(Text, Text)])
getCancer = do
    bs <- liftIO $ T.runHttp $ req
        GET
        (https "polochon.lelele.io" /: "cancer" /: "quickcancer")
        NoReqBody
        bsResponse
        mempty
    let decoded = decodeUtf8' (responseBody bs)
    case decoded of
        Left parseError -> do
            liftIO
                $  putStrLn
                $  "Error decoding quickcancer response: "
                <> show parseError
            return Nothing
        Right rawCancer -> case parseCancer rawCancer of
            Left parseError -> do
                liftIO . print $ "Error: " <> parseErrorPretty parseError
                return Nothing
            Right parsed -> return $ Just parsed


getRandomCancer :: [(Text, Text)]
                -> IRC.StatefulIRC T.BotState (Maybe (Text, Text))
getRandomCancer cancers = do
    let l = length cancers
    if l == 0
        then return Nothing
        else do
            stdGenT <- T._stdGen <$> IRC.state
            idx <-
                liftIO . Conc.atomically $
                do gen <- Conc.readTVar stdGenT
                   let (a, gen') = randomR (0, l - 1) gen
                   Conc.writeTVar stdGenT gen'
                   return a
            return $ Just $ cancers !! idx

getOneCancer :: Text -> [(Text, Text)] -> Maybe (Text, Text)
getOneCancer substr cancers =
    let substrLow = toLower substr
        filtered = filter (isInfixOf substrLow . toLower . fst) cancers
    in if null filtered
           then Nothing
           else Just $ head filtered
