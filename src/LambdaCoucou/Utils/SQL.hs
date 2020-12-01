{-# LANGUAGE BlockArguments #-}
module LambdaCoucou.Utils.SQL where

import qualified Database.SQLite.Simple as SQL
import RIO

withConnection :: String -> (SQL.Connection -> IO a) -> IO a
withConnection filepath action = SQL.withConnection filepath $ \conn -> do
  -- To avoid fatal errors with "database locked"
  -- https://www.sqlite.org/pragma.html#pragma_busy_timeout
  -- This fixes: https://github.com/CoucouInc/lambdaCoucou/issues/27
  SQL.execute_ conn "PRAGMA busy_timeout = 100"
  action conn
