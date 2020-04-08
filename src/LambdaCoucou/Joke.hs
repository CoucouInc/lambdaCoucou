module LambdaCoucou.Joke where

import qualified LambdaCoucou.HandlerUtils as LC.Hdl
import qualified LambdaCoucou.Http as LC.Http
import qualified LambdaCoucou.State as LC.St
import qualified Network.HTTP.Req as Req
import qualified Network.IRC.Client as IRC.C
import RIO
import qualified RIO.Text as T

jokeCommandHandler :: Maybe Text -> IRC.C.IRC LC.St.CoucouState (Maybe Text)
jokeCommandHandler target = do
  res <- liftIO fetchDadJoke
  pure $ Just $ LC.Hdl.addTarget target res

fetchDadJoke :: MonadIO m => m Text
fetchDadJoke = do
  result <-
    LC.Http.runFetch $
      Req.req
        Req.GET
        (Req.https "icanhazdadjoke.com")
        Req.NoReqBody
        Req.bsResponse
        ( Req.header "User-Agent" "IRC bot https://github.com/CoucouInc/lambdaCoucou/"
            <> Req.header "Accept" "text/plain"
        )
  case result of
    Left err -> pure $ LC.Http.showHttpException err
    Right raw -> case T.decodeUtf8' (Req.responseBody raw) of
      Left _err -> pure "Invalid unicode response from the third party :/"
      Right res -> pure res
