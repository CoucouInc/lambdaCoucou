{-# LANGUAGE StrictData #-}

module LambdaCoucou.Crypto where

import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import qualified Control.Monad.Except as Ex
import Data.Scientific
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.Ok as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified LambdaCoucou.HandlerUtils as LC.Hdl
import qualified LambdaCoucou.Http as LC.Http
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as Req
import qualified Network.IRC.Client as IRC.C
import RIO
import qualified RIO.Text as T

cryptoCommandHandler ::
  Either Text CryptoCoin ->
  Maybe Text ->
  IRC.C.IRC s (Maybe Text)
cryptoCommandHandler eiCoin mbTarget =
  case eiCoin of
    Left x -> pure $ Just $ showUnknownCoin x
    Right coin ->
      Just . LC.Hdl.addTarget mbTarget . either showCryptoError (showRate coin)
        <$> getRateInEuro coin

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

exchange :: CryptoCoin -> Text
exchange = \case
  Bitcoin -> "bitstamp"
  Ethereum -> "bitstamp"

symbol :: CryptoCoin -> Text
symbol = \case
  Bitcoin -> "btc"
  Ethereum -> "eth"

newtype CryptoPrice = CryptoPrice { getCryptoPrice :: Scientific }
  deriving (Show)

instance JSON.FromJSON CryptoPrice where
  parseJSON = JSON.withObject "result" $ \result -> do
    res <- result .: "result"
    JSON.withObject "price"
      (\price -> CryptoPrice <$> price .: "price")
      res

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

showRate :: CryptoCoin -> Scientific -> Text
showRate coin val =
  "1 "
    <> symbol coin
    <> " vaut "
    <> T.pack (show val)
    <> " euros grâce au pouvoir de la spéculation !"

showUnknownCoin :: Text -> Text
showUnknownCoin txt = "Dénomination inconnue: " <> txt <> ". Ici on ne deal qu'avec des monnaies respectueuses comme: btc and eth"

------------------------------------------------------------
-- Persistence

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
