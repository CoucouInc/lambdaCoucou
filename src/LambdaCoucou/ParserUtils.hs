module LambdaCoucou.ParserUtils where

import qualified Data.Char as Chr
import RIO
import qualified RIO.Text as T
import qualified RIO.Vector as V
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C

type Parser = M.Parsec Void Text

word :: Parser Text
word = T.pack <$> M.some C.letterChar

-- issue #17 a bit lame of a fix, but parsing urls is already complex enough
-- so simply look for potential surrounding tokens
urlReservedChar :: Vector Char
urlReservedChar =
  V.fromList
    [ '(',
      ')',
      '[',
      ']',
      '>'
    ]

urlWord :: Parser Text
urlWord = T.pack <$> M.some (M.satisfy (\c -> not (Chr.isSpace c) && c `notElem` urlReservedChar))

-- parses a word with almost anything inside, except the specifier for a target '>'
utf8Word' :: Parser Text
utf8Word' = T.pack <$> M.some (M.satisfy (\c -> not (Chr.isSpace c) && c /= '>'))

utf8Word :: Parser Text
utf8Word = T.pack <$> M.some (M.satisfy (not . Chr.isSpace))

spaces :: Parser ()
spaces = void (M.many C.spaceChar)
