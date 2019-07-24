module Main where

import qualified LambdaCoucou.Client as Client

main :: IO ()
main = Client.test *> putStrLn "done"
