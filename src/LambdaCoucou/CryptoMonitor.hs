{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LambdaCoucou.CryptoMonitor where

import qualified Data.Foldable as F
import qualified Control.Exception as Ex
import qualified Network.IRC.Client.Events as IRC.Ev
import qualified Network.IRC.Client               as IRC.C
import           Control.Concurrent               (threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.List                        as List
import           Data.Scientific
import qualified Data.Scientific                  as S
import           Data.Text                        (Text)
import qualified Data.Text                        as Tx
import qualified Data.Text.IO                     as Tx.IO
import qualified Data.Time.Clock                  as T
import qualified Database.SQLite.Simple           as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.Ok        as SQL
import qualified Database.SQLite.Simple.ToField   as SQL
import           Text.ParserCombinators.ReadP     (readP_to_S)

import qualified LambdaCoucou.Crypto              as Crypto

data Rate = Rate
  { rDate       :: T.UTCTime
  , rCryptoCoin :: Crypto.CryptoCoin
  , rCryptoRate :: SQLRate
  }
  deriving (Show, Eq)

newtype SQLRate = SQLRate { getSQLRate :: Scientific }
  deriving (Show, Eq)

instance SQL.ToField SQLRate where
  toField c = SQL.SQLText (Tx.pack $ show $ getSQLRate c)

instance SQL.FromField SQLRate where
  fromField f = case SQL.fieldData f of
    SQL.SQLText txt -> case readMay txt of
      Just x -> SQL.Ok (SQLRate x)
      Nothing -> SQL.returnError SQL.ConversionFailed f
        ("Unexpected scientific value: " <> Tx.unpack txt)
    _ -> SQL.returnError SQL.Incompatible f "Cannot parse SQLRate value"

    where
      readMay :: Text -> Maybe Scientific
      readMay txt = fst <$> List.find (null . snd) (readP_to_S S.scientificP (Tx.unpack txt))

showPretty :: Rate -> Text
showPretty r
  = Tx.pack (show $ rCryptoCoin r)
  <> " rate at " <> Tx.pack (show $ rDate r)
  <> ": " <> Tx.pack (show $ getSQLRate $ rCryptoRate r)

instance SQL.ToRow Rate where
  toRow r =
    [ SQL.toField (rDate r)
    , SQL.toField (rCryptoCoin r)
    , SQL.toField (rCryptoRate r)
    ]

instance SQL.FromRow Rate where
  fromRow = Rate
    <$> SQL.field
    <*> SQL.field
    <*> SQL.field

saveRate :: SQL.Connection -> Rate -> IO ()
saveRate conn =
  SQL.execute conn "INSERT INTO crypto_rate (date, coin, rate) VALUES (?, ?, ?)"


getRateAndSave
  :: (MonadIO m)
  => FilePath
  -> Crypto.CryptoCoin
  -> m ()

getRateAndSave fp coin = Crypto.getRateInEuro coin >>= \case
  Left err -> liftIO $ Tx.IO.putStrLn $ "Error while fetching rate: " <> Crypto.showCryptoError err
  Right rate -> void $ liftIO $ SQL.withConnection fp $ \conn -> do
    SQL.execute_ conn "CREATE TABLE IF NOT EXISTS crypto_rate(date DATETIME, coin TEXT, rate TEXT)"
    now <- T.getCurrentTime
    let rateRecord = Rate
          { rDate       = now
          , rCryptoCoin = coin
          , rCryptoRate = SQLRate rate
          }
    saveRate conn rateRecord
    Tx.IO.putStrLn $ "cryptocoin rate saved: " <> showPretty rateRecord


monitorRates :: FilePath -> IRC.C.IRC s ()
monitorRates fp = forever $ do
  res <- liftIO $ Ex.try (F.traverse_ (getRateAndSave fp) [Crypto.Bitcoin, Crypto.Ethereum])
  case res of
    Right _ -> pure ()
    Left err -> do
      let msg = "ERROR: " <> Tx.pack (show @Ex.SomeException err)
      liftIO $ Tx.IO.putStrLn msg
      IRC.C.send $ IRC.Ev.Privmsg
        "Geekingfrog"
        (Right msg)
  liftIO $ threadDelay (3600 * 10 ^ 6)
