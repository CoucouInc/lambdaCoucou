{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Parser where

import Data.Char (isSpace)
import Text.Megaparsec
import Text.Megaparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

import LambdaCoucou.Types (CoucouCmd(..), CmdFactoidType(..))

testStuff :: Text -> IO ()
testStuff = print . parseCommand

parseCommand :: Text -> Either (ParseError Char Dec) CoucouCmd
parseCommand raw = parse commandParser "" raw

-- if it doesn't start with the special character, ignore the command
commandParser :: Parser CoucouCmd
commandParser = (char 'λ' *> commandParser' <* space) <|> pure CoucouCmdNop

commandParser' :: Parser CoucouCmd
commandParser' = try cancer <|> factoid

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
counterFactoid = do
    name <- factoidName (string "++" <|> string "--")
    space
    (string "++" >> return (CoucouCmdFactoid name IncFactoid) <* space <* eof) <|>
        (string "--" >> return (CoucouCmdFactoid name DecFactoid) <* space <* eof)

setFactoid :: Parser CoucouCmd
setFactoid = do
    let operatorParser = string "=" <|> string ":=" <|> string "+="
    name <- factoidName operatorParser
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

factoidName :: Parser String -> Parser Text
factoidName limit =
    T.pack <$>
    someTill (satisfy (not . isSpace)) (try $ space >> lookAhead limit)

getFactoid :: Parser CoucouCmd
getFactoid = do
    name <- T.pack <$> some (satisfy (not . isSpace))
    space
    eof
    return $ CoucouCmdFactoid name GetFactoid
