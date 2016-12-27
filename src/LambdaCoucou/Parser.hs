{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Parser where

import Data.Char (isSpace)
import Text.Megaparsec
import Text.Megaparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

import LambdaCoucou.Types (CoucouCmd(..), CmdFactoidType(..))

type CoucouParser a = Parsec Dec Text a

testStuff :: Text -> IO ()
testStuff = print . parseCommand

parseCommand :: Text -> Either (ParseError Char Dec) CoucouCmd
parseCommand raw = parse commandParser "" raw

-- if it doesn't start with the special character, ignore the command
commandParser :: CoucouParser CoucouCmd
commandParser = (char 'λ' *> commandParser' <* space) <|> pure CoucouCmdNop

commandParser' :: CoucouParser CoucouCmd
commandParser' = try cancer <|> factoid

cancer :: CoucouParser CoucouCmd
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

factoid :: CoucouParser CoucouCmd
factoid = do
    name <- T.pack <$> some (satisfy (not . isSpace))
    (string "++" >> return (CoucouCmdFactoid name IncFactoid)) <|>
        (string "--" >> return (CoucouCmdFactoid name DecFactoid)) <|>
        do space
           factoidType <- setFactoid <|> return GetFactoid
           return $ CoucouCmdFactoid name factoidType

setFactoid :: CoucouParser CmdFactoidType
setFactoid = do
    operator <- string "=" <|> string ":=" <|> string "+="
    space
    val <- T.pack <$> many anyChar
    if T.null val
        then return DeleteFactoid
        else do
            let op
                    | operator == "=" = SetFactoid
                    | operator == ":=" = ResetFactoid
                    | otherwise = AugmentFactoid
            return $ op val
