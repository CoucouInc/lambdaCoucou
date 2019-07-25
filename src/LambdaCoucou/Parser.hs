{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.Parser where

import           Control.Applicative  hiding (many, some)
import           Control.Monad
import qualified Data.Char            as Chr
import           Data.Text            (Text)
import qualified Data.Text            as Tx
import           Data.Void            (Void)
import qualified Text.Megaparsec      as M
import qualified Text.Megaparsec.Char as C

import qualified LambdaCoucou.Command as LC.Cmd

type Parser = M.Parsec Void Text

hush :: Either e a -> Maybe a
hush = \case
  Left _ -> Nothing
  Right x -> Just x


parseCommand :: Text -> Either (M.ParseErrorBundle Text Void) LC.Cmd.CoucouCmd
parseCommand = M.parse commandParser "cmdParser"

commandParser :: Parser LC.Cmd.CoucouCmd
commandParser = prefix *> M.choice
  [ M.try urlCommandParser
  ]
  <|> pure LC.Cmd.Nop

-- commandParser = try (prefix *> commandParser' <* space) <|> try incCoucou <|> pure CoucouCmdNop

prefix :: Parser Char
prefix = C.char 'λ' <|> C.char '&' <|> C.char 'Σ' -- Σ for sigma_g

urlCommandParser :: Parser LC.Cmd.CoucouCmd
urlCommandParser = do
  C.string "url"
  LC.Cmd.Url <$> targetParser

targetParser :: Parser (Maybe Text)
targetParser =
  ( M.some C.spaceChar
    *> C.char '>'
    *> M.some C.spaceChar
    *> fmap Just word
    <* M.eof
  )
  <|> pure Nothing

parseUrl :: Text -> Maybe Text
parseUrl = hush . M.parse url ""

url :: Parser Text
url
  = (M.eof *> fail "eof url")
  <|> (spaces *> (M.try url' <|> word *> url)) --  <|> (word *> spaces *> url))

url' :: Parser Text
url' = do
  proto <- C.string' "http://" <|> C.string' "https://"
  rest <- M.some (M.satisfy (not . Chr.isSpace))
  pure $ proto <> Tx.pack rest

word :: Parser Text
word = Tx.pack <$> M.some C.letterChar

spaces :: Parser ()
spaces = void (M.many C.spaceChar)
