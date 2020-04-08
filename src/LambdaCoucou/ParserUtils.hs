module LambdaCoucou.ParserUtils where

import RIO
import qualified RIO.Text as T

import qualified Text.Megaparsec      as M
import qualified Text.Megaparsec.Char as C

import qualified Data.Char            as Chr

type Parser = M.Parsec Void Text

word :: Parser Text
word = T.pack <$> M.some C.letterChar

-- parses a word with almost anything inside, except the specifier for a target '>'
utf8Word' :: Parser Text
utf8Word' = T.pack <$> M.some (M.satisfy (\c -> not (Chr.isSpace c) && c /= '>'))

utf8Word :: Parser Text
utf8Word = T.pack <$> M.some (M.satisfy (not . Chr.isSpace))

spaces :: Parser ()
spaces = void (M.many C.spaceChar)
