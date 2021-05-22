{-# LANGUAGE StrictData #-}

module LambdaCoucou.Cli where

import qualified Options.Applicative as O
import RIO

data Config = Config
  { hostname :: ByteString,
    port :: Int,
    chan :: Text,
    nick :: Text,
    sqlitePath :: FilePath
  }
  deriving (Generic)

configOptions :: O.Parser Config
configOptions =
  Config
    <$> ( O.strOption
            ( O.long "hostname"
                <> O.help "Hostname of the irc server to connect to"
                <> O.value "irc.libera.chat"
            )
        )
    <*> ( O.option
            O.auto
            ( O.long "port"
                <> O.help "Port to connect to (assume TLS)"
                <> O.value 6697
            )
        )
    <*> ( O.strOption
            ( O.long "chan"
                <> O.short 'c'
                <> O.help "Channel to connect to"
                <> O.value "#gougoutest"
            )
        )
    <*> ( O.strOption
            ( O.long "nick"
                <> O.short 'n'
                <> O.help "Nickname of the bot"
                <> O.value "testLambdacoucou"
            )
        )
    <*> ( O.strOption
            ( O.long "sqlite-filepath"
                <> O.help "Filepath to the sqlite db"
            )
        )

opts =
  O.info
    (configOptions O.<**> O.helper)
    ( O.fullDesc
        <> O.progDesc "Industrial strength IRC bot for more coucou"
        <> O.header "IRC bot"
    )

getConfig :: IO Config
getConfig = O.execParser opts
