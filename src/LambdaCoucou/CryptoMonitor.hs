module LambdaCoucou.CryptoMonitor where

import Data.Scientific
import qualified Data.Text.IO as T.IO
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.Ok as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified LambdaCoucou.Crypto as Crypto
import qualified Network.IRC.Client as IRC.C
import qualified Network.IRC.Client.Events as IRC.Ev
import RIO
import qualified RIO.Text as T
import qualified RIO.Time as Time

data Rate
  = Rate
      { rDate :: Time.UTCTime,
        rCryptoCoin :: Crypto.CryptoCoin,
        rCryptoRate :: SQLRate
      }
  deriving (Show, Eq)

newtype SQLRate = SQLRate {getSQLRate :: Scientific}
  deriving (Show, Eq)

instance SQL.ToField SQLRate where
  toField c = SQL.SQLText (T.pack $ show $ getSQLRate c)

instance SQL.FromField SQLRate where
  fromField f = case SQL.fieldData f of
    SQL.SQLText txt -> case readMaybe (T.unpack txt) of
      Just x -> SQL.Ok (SQLRate x)
      Nothing ->
        SQL.returnError
          SQL.ConversionFailed
          f
          ("Unexpected scientific value: " <> T.unpack txt)
    _ -> SQL.returnError SQL.Incompatible f "Cannot parse SQLRate value"

showPretty :: Rate -> Text
showPretty r =
  T.pack (show $ rCryptoCoin r)
    <> " rate at "
    <> T.pack (show $ rDate r)
    <> ": "
    <> T.pack (show $ getSQLRate $ rCryptoRate r)

instance SQL.ToRow Rate where
  toRow r =
    [ SQL.toField (rDate r),
      SQL.toField (rCryptoCoin r),
      SQL.toField (rCryptoRate r)
    ]

instance SQL.FromRow Rate where
  fromRow =
    Rate
      <$> SQL.field
      <*> SQL.field
      <*> SQL.field

saveRate :: SQL.Connection -> Rate -> IO ()
saveRate conn =
  SQL.execute conn "INSERT INTO crypto_rate (date, coin, rate) VALUES (?, ?, ?)"

getRateAndSave ::
  (MonadIO m) =>
  FilePath ->
  Crypto.CryptoCoin ->
  m ()
getRateAndSave fp coin = Crypto.getRateInEuro coin >>= \case
  Left err -> liftIO $ T.IO.putStrLn $ "Error while fetching rate: " <> Crypto.showCryptoError err
  Right rate -> void $ liftIO $ SQL.withConnection fp $ \conn -> do
    SQL.execute_ conn "CREATE TABLE IF NOT EXISTS crypto_rate(date DATETIME, coin TEXT, rate TEXT)"
    now <- Time.getCurrentTime
    let rateRecord =
          Rate
            { rDate = now,
              rCryptoCoin = coin,
              rCryptoRate = SQLRate rate
            }
    saveRate conn rateRecord
    T.IO.putStrLn $ "cryptocoin rate saved: " <> showPretty rateRecord

monitorRates :: FilePath -> IRC.C.IRC s ()
monitorRates fp = forever $ do
  res <- liftIO $ try (traverse_ (getRateAndSave fp) [Crypto.Bitcoin, Crypto.Ethereum])
  case res of
    Right _ -> pure ()
    Left err -> do
      let msg = "ERROR: " <> T.pack (show @SomeException err)
      liftIO $ T.IO.putStrLn msg
      IRC.C.send $
        IRC.Ev.Privmsg
          "Geekingfrog"
          (Right msg)
  liftIO $ threadDelay (3600 * 10 ^ 6)
