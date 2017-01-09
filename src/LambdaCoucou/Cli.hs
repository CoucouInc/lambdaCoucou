module LambdaCoucou.Cli where

import Data.Monoid ((<>))
import Options.Applicative
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (pack)

data Opts = Opts
    { optHost :: !ByteString
    , optPort :: !Int
    , optNick :: !Text
    , optChan :: !Text
    }
    deriving (Show)

parseOptions :: IO Opts
parseOptions = execParser optsParser

optsParser :: ParserInfo Opts
optsParser =
    info
        (helper <*> programOptions)
        (fullDesc <> progDesc "λcoucou" <> header "Industrial strength irc bot for more coucou")
  where
    programOptions = Opts <$> hostOption <*> portOption <*> nickOption <*> chanOption
    hostOption =
        BS.pack <$>
        strOption
            (long "host" <> short 'h' <> metavar "HOST" <> value "chat.freenode.net" <>
             help "Hostname of irc server")
    portOption =
        option auto (long "port" <> short 'p' <> metavar "PORT" <> value 6697 <> help "Port")
    nickOption =
        T.pack <$>
        strOption
            (long "nick" <> short 'n' <> metavar "NICK" <> value "lambdacoucou" <>
             help "Nick of the bot")
    chanOption =
        T.pack <$>
        strOption
            (long "channel-name" <> short 'c' <> metavar "CHAN" <> value "#arch-fr-free" <>
             help "Channel to connect to")
