module LambdaCoucou.PR where

import qualified LambdaCoucou.HandlerUtils as LC.Hdl
import qualified Network.IRC.Client as IRC.C
import RIO

prCommandHandler ::
  Maybe Text ->
  IRC.C.IRC state (Maybe Text)
prCommandHandler target = do
  let msg = "PR welcome: https://github.com/CoucouInc/lambdaCoucou/"
  pure $ Just $ LC.Hdl.addTarget target msg
