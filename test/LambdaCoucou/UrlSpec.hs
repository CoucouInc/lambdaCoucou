{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.UrlSpec where

import qualified Test.Hspec              as H
import qualified Test.Hspec.Expectations as HE
import qualified Data.Text.IO            as Tx.IO

import qualified LambdaCoucou.Url as Url

tests :: H.SpecWith ()
tests = H.describe "Url" $ do
  H.describe "parseYoutubeIdLong" $ do
    H.it "works" $ do
      let ytId = "zt0OQb1DBko"
      Url.parseYoutubeIdLong ("https://www.youtube.com/watch?v=" <> ytId)
        `HE.shouldBe` Just ytId

    H.it "doesn't care about the format of the id" $ do
      let ytId = "deadbeefbar"
      Url.parseYoutubeIdLong ("https://www.youtube.com/watch?v=" <> ytId)
        `HE.shouldBe` Just ytId

    H.it "parses query params properly" $ do
      let ytId = "foo"
      Url.parseYoutubeIdLong ("https://www.youtube.com/watch?foo=bar&v=" <> ytId <> "&moo=qux")
        `HE.shouldBe` Just ytId

  H.describe "parseYoutubeIdShort" $ do
    H.it "works" $ do
      let ytId = "qp7t5ZW9fm4"
      Url.parseYoutubeIdShort ("https://youtu.be/" <> ytId)
        `HE.shouldBe` Just ytId

    H.it "ignores query params" $ do
      let ytId = "foo"
      Url.parseYoutubeIdShort ("https://youtu.be/" <> ytId <> "?t=bar")
        `HE.shouldBe` Just ytId

    H.it "works for www" $ do
      let ytId = "foo"
      Url.parseYoutubeIdShort ("https://www.youtu.be/" <> ytId)
        `HE.shouldBe` Just ytId

  H.describe "parseTitle" $
    H.it "parses capitalized tags" $ do
      raw <- Tx.IO.readFile "./test/resources/capitalised-tags.html"
      Url.parseTitle raw `HE.shouldBe` Just " target title "
