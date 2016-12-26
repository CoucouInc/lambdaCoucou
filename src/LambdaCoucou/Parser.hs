{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Parser where

import Text.Megaparsec
import Text.Megaparsec.Text
import Data.Text (Text, pack)

import LambdaCoucou.Types (CoucouCmd(..))

type CoucouParser a = Parsec Dec Text a

testStuff :: Text -> IO ()
testStuff = print . parseCommand

parseCommand :: Text -> Either (ParseError Char Dec) CoucouCmd
parseCommand raw = parse commandParser "" raw

-- if it doesn't start with the special character, ignore the command
commandParser :: CoucouParser CoucouCmd
commandParser = (char 'λ' *> commandParser' <* space) <|> pure CoucouCmdNop


commandParser' :: CoucouParser CoucouCmd
commandParser' = cancer


cancer :: CoucouParser CoucouCmd
cancer = do
    string "cancer"
    space
    search <- Just . pack <$> some (alphaNumChar <|> spaceChar) <|> return Nothing
    return $ CoucouCmdCancer search



parseCancer :: Text -> Either (ParseError Char Dec) [(Text, Text)]
parseCancer raw = parse cancerParser "" raw

cancerParser :: Parsec Dec Text [(Text, Text)]
cancerParser = many cancerLine

cancerLine :: Parsec Dec Text (Text, Text)
cancerLine = do
    title <- pack <$> many (noneOf [':'])
    string ": "
    url <- pack <$> manyTill anyChar eol
    return (title, url)
