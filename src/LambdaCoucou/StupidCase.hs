module LambdaCoucou.StupidCase where

import qualified LambdaCoucou.HandlerUtils as LC.Hdl
import qualified LambdaCoucou.State as LC.St
import qualified Network.IRC.Client as IRC.C
import RIO
import qualified RIO.Text as T
import qualified Data.Char as C

stupidCommandHandler :: [Text] -> Maybe Text -> IRC.C.IRC LC.St.CoucouState (Maybe Text)
stupidCommandHandler words target = do
  pure $ Just $ LC.Hdl.addTarget target (stupidTransformList words True)

stupidTransformList :: [Text] -> Bool -> Text
stupidTransformList [] _ = ""
stupidTransformList (x:xs) b =
  let (newb, newx) = stupidTransform x b in
  T.intercalate " " [newx, stupidTransformList xs newb]

stupidTransform word b = T.mapAccumL (\b c -> (not b, if b then C.toUpper c else C.toLower c)) b word


