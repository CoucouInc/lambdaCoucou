{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.HandlerUtils where

import Data.Text (Text)

addTarget :: Maybe Text -> Text -> Text
addTarget mbTarget msg = case mbTarget of
  Nothing -> msg
  Just x  -> x <> ": " <> msg
