{-# LANGUAGE StrictData #-}

module LambdaCoucou.Cli where

import qualified Options.Applicative as O
import RIO

data Config = Config
  { chan :: Text,
    nick :: Text,
    sqlitePath :: FilePath
  }
  deriving (Generic)

configOptions :: O.Parser Config
configOptions =
  Config
    <$> ( O.strOption
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

opts = O.info
  (configOptions O.<**> O.helper)
  (O.fullDesc
    <> O.progDesc "Industrial strength IRC bot for more coucou"
    <> O.header "IRC bot"
  )

getConfig :: IO Config
getConfig = O.execParser opts
