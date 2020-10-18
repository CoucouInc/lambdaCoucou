{-# LANGUAGE NoImplicitPrelude #-}

module LambdaCoucou.UrlSpec where

import qualified Data.Text.IO as Tx.IO
import qualified LambdaCoucou.Url as Url
import RIO
import qualified Test.Hspec as H
import qualified Test.Hspec.Expectations as HE

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
    H.it "parses capitalized tags" $
      do
        raw <- Tx.IO.readFile "./test/resources/capitalised-tags.html"
        Url.parseTitle raw `HE.shouldBe` Just " target title "

  H.describe "withoutTracker" $ do
    H.it "remove any utm_* tracking shit" $
      Url.withoutTracker "http://foo.bar.com/?utm_source=source&utm_campaign=campaign&utm_medium=medium&utm_term=term&foo&bar=qux&utm_content=content" `HE.shouldBe` Just "http://foo.bar.com/?foo&bar=qux"

    H.it "doesn't do anything for invalid urls" $
      Url.withoutTracker "lolwat" `HE.shouldBe` Nothing

    H.it "handle encoding properly" $
      let u = "https://foo.bar.com/?crid=blahblah&uppercase=foo%2Bbar%2Bbaz&comma=foo%2Cmoo"
      in Url.withoutTracker u `HE.shouldBe` Nothing
