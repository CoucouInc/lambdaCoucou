{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Parser where

import Data.Monoid ((<>))
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
import Text.Megaparsec
import Text.Megaparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

import LambdaCoucou.Types (CoucouCmd(..), CmdFactoidType(..))

parseCommand :: Text -> Either (ParseError Char Dec) CoucouCmd
parseCommand raw = parse commandParser "" raw

-- if it doesn't start with the special character, ignore the command
commandParser :: Parser CoucouCmd
commandParser = try (prefix *> commandParser' <* space) <|> try incCoucou <|> pure CoucouCmdNop

prefix :: Parser Char
prefix = char 'λ' <|> char '>'

commandParser' :: Parser CoucouCmd
commandParser' = try getCoucou <|> try lastSeen <|> try cancer <|> factoid

cancer :: Parser CoucouCmd
cancer = do
    string "cancer"
    space
    search <- Just . T.pack <$> some (alphaNumChar <|> spaceChar) <|> return Nothing
    return $ CoucouCmdCancer search

parseCancer :: Text -> Either (ParseError Char Dec) [(Text, Text)]
parseCancer raw = parse cancerParser "" raw

cancerParser :: Parsec Dec Text [(Text, Text)]
cancerParser = many cancerLine

cancerLine :: Parsec Dec Text (Text, Text)
cancerLine = do
    title <- T.pack <$> many (noneOf [':'])
    string ": "
    url <- T.pack <$> manyTill anyChar eol
    return (title, url)

factoid :: Parser CoucouCmd
factoid = try counterFactoid <|> try setFactoid <|> getFactoid

counterFactoid :: Parser CoucouCmd
counterFactoid = try counterPlus <|> counterMinus

counterPlus :: Parser CoucouCmd
counterPlus = do
    name <- genericCounter (string "++") >>= validFactoidName
    return (CoucouCmdFactoid name IncFactoid)

counterMinus :: Parser CoucouCmd
counterMinus = do
    name <- genericCounter (string "--") >>= validFactoidName
    return (CoucouCmdFactoid name DecFactoid)

genericCounter :: Parser String -> Parser Text
genericCounter limit = T.pack <$> (someTill (satisfy (not . isSpace)) limit <* space <* eof)

setFactoid :: Parser CoucouCmd
setFactoid = do
    let operatorParser = string "=" <|> string ":=" <|> string "+="
    name <- T.pack <$> someTill (satisfy (not . isSpace)) (lookAhead (some spaceChar <|> operatorParser))
    space
    operator <- operatorParser
    space
    val <- T.pack <$> many anyChar
    eof
    if T.null val
        then return $ CoucouCmdFactoid name DeleteFactoid
        else do
            let op
                    | operator == "=" = SetFactoid
                    | operator == ":=" = ResetFactoid
                    | otherwise = AugmentFactoid
            return $ CoucouCmdFactoid name (op val)

validFactoidName :: Text -> Parser Text
validFactoidName name =
    if name `elem` factoidBlackList
        then fail $ T.unpack $ "Reserved keyword " <> name
        else return name

-- reserved name, these cannot be used for factoids
factoidBlackList :: [Text]
factoidBlackList = ["seen"]

factoidName :: Maybe (Parser String) -> Parser Text
factoidName mbLimit = do
    let limit = fromMaybe (many spaceChar) mbLimit
    name <- T.pack <$> someTill (satisfy (not . isSpace)) (try $ lookAhead (some spaceChar <|> limit))
    if name `elem` factoidBlackList
    then fail $ T.unpack $ "Reserved keyword " <> name
    else return name

word :: Parser Text
word = T.pack <$> manyTill (satisfy (not . isSpace)) (void spaceChar <|> eof)

getFactoid :: Parser CoucouCmd
getFactoid = do
    name <- word >>= validFactoidName
    space
    mbHl <- optional (char '>' >> space >> word)
    space
    eof
    return $ CoucouCmdFactoid name (GetFactoid mbHl)


getCoucou :: Parser CoucouCmd
getCoucou = do
    string "coucou"
    target <-
        try (many spaceChar *> eof *> return Nothing) -- (only space afterwards)
         <|>
        (some spaceChar >> (Just <$> nick)) -- (at least one space then a nick)
    return $ CoucouCmdGetCoucou target

incCoucou :: Parser CoucouCmd
incCoucou = do
    space
    try (string "coucou" <* (eof <|> void spaceChar)) <|>
        manyTill anyChar (string " coucou" <* (eof <|> void (some spaceChar)))
    return CoucouCmdIncCoucou

nick :: Parser Text
nick = T.pack <$> some (satisfy (not . isSpace))

lastSeen :: Parser CoucouCmd
lastSeen = do
    string "seen"
    some spaceChar
    n <- nick
    return $ CoucouCmdLastSeen n
