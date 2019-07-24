module LambdaCoucou.Cli where

import Data.Monoid ((<>))
import Options.Applicative
import qualified Data.Text as T (pack)
import qualified Data.ByteString.Char8 as BS (pack)
import LambdaCoucou.Types (Opts(..))
import LambdaCoucou.Run (run)

entry :: IO ()
entry = execParser optsParser >>= run

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
