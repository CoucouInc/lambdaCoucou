{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.ParserSpec (main, spec) where

import Data.Monoid ((<>))
import Test.Hspec
import Test.Hspec.Megaparsec

import Text.Megaparsec (parse)
import qualified LambdaCoucou.Parser as P
import qualified LambdaCoucou.Types as T

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

-- spec :: Spec
-- spec = do
--     describe "foo" $ do
--         it "works" $ do
--             True `shouldBe` True
--         it "still works" $ do
--             False `shouldBe` False

spec :: Spec
spec = do
    describe "coucou command parser" $ do
        it "parses simple coucou string" $ do
            P.parseCommand "λcoucou" `shouldParse` T.CoucouCmdGetCoucou Nothing
            P.parseCommand "λcoucou  " `shouldParse` T.CoucouCmdGetCoucou Nothing
        it "parse even if blank before" $
            P.parseCommand "λcoucou  foo" `shouldParse` T.CoucouCmdGetCoucou (Just "foo")

    describe "cancer command parser" $ do
        it "parses simple cancer string" $ do
            P.parseCommand "λcancer" `shouldParse` T.CoucouCmdCancer Nothing Nothing
            P.parseCommand "λcancer  " `shouldParse` T.CoucouCmdCancer Nothing Nothing
        it "parses search term" $
            P.parseCommand "λcancer  foo " `shouldParse` T.CoucouCmdCancer (Just "foo") Nothing
        it "parses hl nick" $
            P.parseCommand "λcancer >  foo " `shouldParse` T.CoucouCmdCancer Nothing (Just "foo")
        it "parses search and hl nick" $
            P.parseCommand "λcancer  search  >  nick " `shouldParse` T.CoucouCmdCancer (Just "search") (Just "nick")
        it "parses factoid with > at the end" $
            P.parseCommand "λcancer foo>" `shouldParse` T.CoucouCmdCancer (Just "foo>") Nothing
        it "only parses one search term" $
            P.parseCommand "λcancer foo bar" `shouldParse` T.CoucouCmdNop


    describe "factoids command parser" $ do

        describe "counters" $ do
            it "parses counter++" $ do
                P.parseCommand "λfoo++" `shouldParse` T.CoucouCmdFactoid "foo" T.IncFactoid
                P.parseCommand "λfoo++  " `shouldParse` T.CoucouCmdFactoid "foo" T.IncFactoid
            it "parses counter--" $ do
                P.parseCommand "λfoo--"  `shouldParse` T.CoucouCmdFactoid "foo" T.DecFactoid
                P.parseCommand "λfoo--  "  `shouldParse` T.CoucouCmdFactoid "foo" T.DecFactoid
            it "only parses one word" $ do
                P.parseCommand "λfoo--  bar"  `shouldParse` T.CoucouCmdNop
                P.parseCommand "λfoo++  bar" `shouldParse` T.CoucouCmdNop

        describe "getting factoids" $ do
            it "parse only one word" $
                parse P.factoid "" `shouldFailOn` "λfoo bar"
            it "fails on blacklisted names" $
                P.parseCommand "λseen" `shouldParse` T.CoucouCmdNop
            it "parses «weird» names" $ do
                let factoid = "γ_éï机器-"
                let expectedCommand = T.CoucouCmdFactoid factoid (T.GetFactoid Nothing)
                P.parseCommand ("λ" <> factoid) `shouldParse` expectedCommand
            it "can hl user with a factoid" $
                P.parseCommand "λfoo > bar" `shouldParse` T.CoucouCmdFactoid "foo" (T.GetFactoid (Just "bar"))
            it "get factoid starting with cancer" $
                P.parseCommand "λcancerd" `shouldParse` T.CoucouCmdFactoid "cancerd" (T.GetFactoid Nothing)


        describe "setting factoids" $ do
            it "can set factoid" $
                P.parseCommand "λfoo = 1" `shouldParse` T.CoucouCmdFactoid "foo" (T.SetFactoid "1")
            it "can reset factoid" $
                P.parseCommand "λfoo := 1" `shouldParse` T.CoucouCmdFactoid "foo" (T.ResetFactoid "1")
            it "can delete factoid" $ do
                P.parseCommand "λfoo :=" `shouldParse` T.CoucouCmdFactoid "foo" T.DeleteFactoid
                P.parseCommand "λfoo :=  " `shouldParse` T.CoucouCmdFactoid "foo" T.DeleteFactoid
            it "can augment factoid" $
                P.parseCommand "λfoo +=  1" `shouldParse` T.CoucouCmdFactoid "foo" (T.AugmentFactoid "1")

    describe "increase coucou count" $ do
        it "picks up coucou at the beginning" $
            P.parseCommand "coucou" `shouldParse` T.CoucouCmdIncCoucou
        it "picks up coucou with spaces around" $ do
            P.parseCommand "foo  coucou  bar" `shouldParse` T.CoucouCmdIncCoucou
            P.parseCommand "  coucou" `shouldParse` T.CoucouCmdIncCoucou
        it "doesn't increase coucou count if not full word" $ do
            parse P.getCoucou "" `shouldFailOn` "foocoucou bar"
            parse P.getCoucou "" `shouldFailOn` "foo coucoubar"

    describe "last seen parser" $ do
        it "parses correct command with nick" $
            P.parseCommand "λseen foonick " `shouldParse` T.CoucouCmdLastSeen "foonick" Nothing
        it "parses correct command with nick and underscore" $
            P.parseCommand "λseen foo_nick " `shouldParse` T.CoucouCmdLastSeen "foo_nick" Nothing
        it "doesn't parse command if no nick follows" $
            P.parseCommand "λseen" `shouldParse` T.CoucouCmdNop
        it "parses hl nick" $
            P.parseCommand "λseen foo_nick  > bar " `shouldParse` T.CoucouCmdLastSeen "foo_nick" (Just "bar")
        it "doesn't parse if only hl nick" $
            P.parseCommand "λseen > foo" `shouldParse` T.CoucouCmdNop
