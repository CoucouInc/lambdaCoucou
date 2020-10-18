module LambdaCoucou.Cancer where

import qualified Control.Monad.Except as Ex
import qualified LambdaCoucou.HandlerUtils as LC.Hdl
import qualified LambdaCoucou.Http as LC.Http
import qualified LambdaCoucou.State as LC.St
import qualified LambdaCoucou.Url as LC.Url
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as Req
import qualified Network.IRC.Client as IRC.C
import RIO
import qualified RIO.Text as T
import RIO.Vector ((!?))
import qualified RIO.Vector as V
import qualified System.Random as Rng

cancerCommandHandler ::
  Maybe LC.St.ChannelName ->
  CancerType ->
  Maybe Text ->
  IRC.C.IRC LC.St.CoucouState (Maybe Text)
cancerCommandHandler mbChanName cancer target = Ex.runExceptT (getCancer cancer) >>= \case
  Left err -> pure $ Just $ showCancerError err
  Right msg -> do
    case mbChanName of
      Nothing -> pure ()
      Just chanName -> void $ LC.Url.updateLastUrl chanName msg
    pure $ Just $ LC.Hdl.addTarget target msg

data CancerError
  = HttpExc Req.HttpException
  | CancerNotFound Text
  | ParseError
  | DecodingError
  deriving (Show)

showCancerError :: CancerError -> Text
showCancerError = \case
  HttpExc exc ->
    LC.Http.showHttpException exc
  CancerNotFound txt ->
    "No cancer found for: " <> txt -- TODO levenstein distance for some suggestions?
  ParseError ->
    "Could not parse the returned list"
  DecodingError ->
    "Returned list is not in valid utf8"

data CancerType
  = RandomCancer
  | SpecificCancer Text
  deriving (Show, Eq)

getCancer ::
  (MonadIO m) =>
  CancerType ->
  Ex.ExceptT CancerError m Text
getCancer cancer = do
  list <- fetchCancerList
  case cancer of
    RandomCancer -> do
      idx <- liftIO $ Rng.randomRIO (0, length list - 1)
      let Just (k, v) = list !? idx
      pure $ k <> ": " <> v
    SpecificCancer c -> do
      let lowC = T.toLower c
      case V.find (T.isInfixOf lowC . T.toLower . fst) list of
        Nothing -> Ex.throwError (CancerNotFound c)
        Just (k, v) -> pure $ k <> ": " <> v

fetchCancerList ::
  MonadIO m =>
  Ex.ExceptT CancerError m (Vector (Text, Text))
fetchCancerList = do
  resp <-
    Ex.withExceptT HttpExc $ LC.Http.runReqMonad $
      Req.req
        Req.GET
        (Req.https "raw.githubusercontent.com" /: "CoucouInc" /: "lalalaliste" /: "master" /: "cancer.txt")
        Req.NoReqBody
        Req.bsResponse
        mempty
  cancerLines <- case decodeUtf8' (Req.responseBody resp) of
    Left _err -> Ex.throwError DecodingError
    Right x -> pure (T.lines x)
  pure $ V.fromList $ fmap parseLine cancerLines

parseLine :: Text -> (Text, Text)
parseLine l =
  let (k, v) = T.span (/= ':') l
   in (k, T.strip (T.drop 1 v))
