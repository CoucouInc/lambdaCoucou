{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module LambdaCoucou.Crypto where

import qualified Control.Monad.Except as Ex
import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import Data.Scientific
import qualified Data.Text.IO as T.IO
import Database.SQLite.Simple (NamedParam (..))
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.Ok as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified LambdaCoucou.HandlerUtils as LC.Hdl
import qualified LambdaCoucou.Http as LC.Http
import qualified LambdaCoucou.State as LC.St
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as Req
import qualified Network.IRC.Client as IRC.C
import qualified Network.IRC.Client.Events as IRC.Ev
import RIO
import qualified RIO.State as St
import qualified RIO.Text as T
import qualified RIO.Time as Time
import System.IO (putStrLn)
import Text.Printf (printf)

data CryptoError
  = CryptoNotFound CryptoCoin
  | HttpExc Req.HttpException
  deriving (Show)

showCryptoError :: CryptoError -> Text
showCryptoError = \case
  CryptoNotFound coin ->
    "Exchange rate not found for " <> symbol coin
  HttpExc exc ->
    LC.Http.showHttpException exc

data CryptoCoin
  = Ethereum
  | Bitcoin
  deriving (Show, Eq)

symbol :: CryptoCoin -> Text
symbol = \case
  Bitcoin -> "btc"
  Ethereum -> "eth"

newtype CryptoPrice = CryptoPrice {getCryptoPrice :: Scientific}
  deriving (Show)

instance JSON.FromJSON CryptoPrice where
  parseJSON = JSON.withObject "result" $ \result -> do
    res <- result .: "result"
    JSON.withObject
      "price"
      (\price -> CryptoPrice <$> price .: "price")
      res

data Rate = Rate
  { rDate :: Time.UTCTime,
    rCryptoCoin :: CryptoCoin,
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

instance SQL.ToField CryptoCoin where
  toField c = SQL.SQLText $ case c of
    Bitcoin -> "BTC"
    Ethereum -> "ETH"

instance SQL.FromField CryptoCoin where
  fromField f = case SQL.fieldData f of
    SQL.SQLText txt -> case txt of
      "BTC" -> SQL.Ok Bitcoin
      "ETH" -> SQL.Ok Ethereum
      _ -> SQL.returnError SQL.ConversionFailed f ("Unexpected value: " <> T.unpack txt)
    _ -> SQL.returnError SQL.Incompatible f "Cannot parse CryptoCoin value"

cryptoCommandHandler ::
  Either Text CryptoCoin ->
  Maybe Text ->
  IRC.C.IRC LC.St.CoucouState (Maybe Text)
cryptoCommandHandler eiCoin mbTarget =
  case eiCoin of
    Left x -> pure $ Just $ showUnknownCoin x
    Right coin ->
      Just . LC.Hdl.addTarget mbTarget . either showCryptoError id -- (showRate coin)
        <$> getRateWithHistory coin

exchange :: CryptoCoin -> Text
exchange = \case
  Bitcoin -> "bitstamp"
  Ethereum -> "bitstamp"

getRateWithHistory ::
  (MonadIO m, St.MonadState LC.St.CoucouState m) =>
  CryptoCoin ->
  m (Either CryptoError Text)
getRateWithHistory coin =
  getRateInEuro coin >>= \case
    Left err -> pure (Left err)
    Right currentRate -> do
      variations <- getVariations coin
      let pretty = prettyVariations currentRate variations
      let hist = if T.null pretty then "" else " (" <> pretty <> ")"
      pure $ Right $ showRate coin currentRate <> hist

getRateInEuro ::
  (MonadIO m) =>
  CryptoCoin ->
  m (Either CryptoError Scientific)
getRateInEuro coin = Ex.runExceptT $ do
  let target = "eur"
  let pair = symbol coin <> target
  bsResp <-
    Ex.withExceptT HttpExc $
      Req.responseBody
        <$> LC.Http.runReqMonad
          ( Req.req
              Req.GET
              (Req.https "api.cryptowat.ch" /: "markets" /: exchange coin /: pair /: "price")
              Req.NoReqBody
              Req.bsResponse
              mempty
          )
  case JSON.decodeStrict bsResp of
    Nothing -> Ex.throwError (CryptoNotFound coin)
    Just (CryptoPrice price) -> pure price

getVariations ::
  (MonadIO m, St.MonadState LC.St.CoucouState m) =>
  CryptoCoin ->
  m (Maybe Rate, Maybe Rate, Maybe Rate)
getVariations coin = do
  sqlitePath <- St.gets LC.St.csSQLitePath
  now <- Time.getCurrentTime
  let dayAgo = changeDays (Time.addDays (-1)) now
  let weekAgo = changeDays (Time.addDays (-7)) now
  let monthAgo = changeDays (Time.addGregorianMonthsRollOver (-1)) now
  rateDayAgo <- liftIO $ getHistoricRate sqlitePath coin dayAgo
  rateWeekAgo <- liftIO $ getHistoricRate sqlitePath coin weekAgo
  rateMonthAgo <- liftIO $ getHistoricRate sqlitePath coin monthAgo
  pure (rateDayAgo, rateWeekAgo, rateMonthAgo)

prettyVariations :: Scientific -> (Maybe Rate, Maybe Rate, Maybe Rate) -> Text
prettyVariations nowRate (mbDayAgo, mbWeekAgo, mbMonthAgo) =
  let withArrow r =
        let pastRate = getSQLRate (rCryptoRate r)
            trf = toRealFloat @Double
            changePc = abs (trf nowRate - trf pastRate) / trf pastRate * 100
         in if nowRate >= pastRate then ("↗", changePc) else ("↘", changePc)
      fmt = T.pack . printf "%.2f"
      day = maybe "" (\(arrow, change) -> arrow <> fmt change <> "% 1D") (fmap withArrow mbDayAgo)
      week = maybe "" (\(arrow, change) -> arrow <> fmt change <> "% 1W") (fmap withArrow mbWeekAgo)
      month = maybe "" (\(arrow, change) -> arrow <> fmt change <> "% 1M") (fmap withArrow mbMonthAgo)
   in case (mbDayAgo, mbWeekAgo, mbMonthAgo) of
        (Nothing, Nothing, Nothing) -> ""
        _ -> day <> " − " <> week <> " − " <> month

changeDays :: (Time.Day -> Time.Day) -> Time.UTCTime -> Time.UTCTime
changeDays f time = time {Time.utctDay = f (Time.utctDay time)}

showRate :: CryptoCoin -> Scientific -> Text
showRate coin val =
  "1 "
    <> symbol coin
    <> " vaut "
    <> T.pack (show val)
    <> " euros grâce au pouvoir de la spéculation !"

showUnknownCoin :: Text -> Text
showUnknownCoin txt = "Dénomination inconnue: " <> txt <> ". Ici on ne deal qu'avec des monnaies respectueuses comme: btc (aka xbt) et eth."

------------------------------------------------------------
-- Persistence

saveRate :: SQL.Connection -> Rate -> IO ()
saveRate conn =
  SQL.execute conn "INSERT INTO crypto_rate (date, coin, rate) VALUES (?, ?, ?)"

createTable :: MonadIO m => FilePath -> m ()
createTable fp = liftIO $
  SQL.withConnection fp $ \conn ->
    SQL.execute_ conn "CREATE TABLE IF NOT EXISTS crypto_rate(date DATETIME, coin TEXT, rate TEXT)"

getRateAndSave ::
  (MonadIO m) =>
  FilePath ->
  CryptoCoin ->
  m ()
getRateAndSave fp coin =
  getRateInEuro coin >>= \case
    Left err -> liftIO $ T.IO.putStrLn $ "Error while fetching rate: " <> showCryptoError err
    Right rate -> void $
      liftIO $
        SQL.withConnection fp $ \conn -> do
          now <- Time.getCurrentTime
          let rateRecord =
                Rate
                  { rDate = now,
                    rCryptoCoin = coin,
                    rCryptoRate = SQLRate rate
                  }
          saveRate conn rateRecord
          T.IO.putStrLn $ "cryptocoin rate saved: " <> showPretty rateRecord

showPretty :: Rate -> Text
showPretty r =
  T.pack (show $ rCryptoCoin r)
    <> " rate at "
    <> T.pack (show $ rDate r)
    <> ": "
    <> T.pack (show $ getSQLRate $ rCryptoRate r)

monitorRates :: FilePath -> IRC.C.IRC s ()
monitorRates fp = forever do
  res <- liftIO $ try (traverse_ (getRateAndSave fp) [Bitcoin, Ethereum])
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

-- | will return the first rate after the given date
getHistoricRate ::
  FilePath ->
  CryptoCoin ->
  Time.UTCTime ->
  IO (Maybe Rate)
getHistoricRate fp coin time = SQL.withConnection fp $ \conn -> do
  result <-
    SQL.queryNamed
      conn
      "SELECT date, coin, rate FROM crypto_rate WHERE date >= :date AND coin = :coin ORDER BY date ASC LIMIT 1"
      [":date" := time, ":coin" := coin]
  case result of
    [] -> pure Nothing
    (x : _) -> pure (Just x)

test :: IO ()
test = do
  now <- Time.getCurrentTime
  let yesterday = now {Time.utctDay = Time.addDays (-1) (Time.utctDay now)}
  putStrLn $ show yesterday
  let path = "./lambdacoucou.sqlite"
  getHistoricRate path Bitcoin now >>= putStrLn . show
  getHistoricRate path Bitcoin yesterday >>= putStrLn . show
  putStrLn "done"
