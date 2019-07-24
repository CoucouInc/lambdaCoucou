{-# LANGUAGE OverloadedStrings #-}

module LambdaCoucou.ParserSpec (main, spec) where

import Data.Monoid ((<>))
import Test.Hspec
import Test.Hspec.Megaparsec

import Text.Megaparsec (parse)
import qualified LambdaCoucou.Parser as P
import LambdaCoucou.Command (CoucouCmd(..), CmdFactoidType(..))
import LambdaCoucou.Help (CoucouHelpType(..))
import LambdaCoucou.Crypto (CryptoCoin(..))

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "coucou command parser" $ do
        it "parses simple coucou string" $ do
            P.parseCommand "λcoucou" `shouldParse` CoucouCmdGetCoucou Nothing
            P.parseCommand "λcoucou  " `shouldParse` CoucouCmdGetCoucou Nothing
        it "parse even if blank before" $
            P.parseCommand "λcoucou  foo" `shouldParse` CoucouCmdGetCoucou (Just "foo")

    describe "cancer command parser" $ do
        it "parses simple cancer string" $ do
            P.parseCommand "λcancer" `shouldParse` CoucouCmdCancer Nothing Nothing
            P.parseCommand "λcancer  " `shouldParse` CoucouCmdCancer Nothing Nothing
        it "parses search term" $
            P.parseCommand "λcancer  foo " `shouldParse` CoucouCmdCancer (Just "foo") Nothing
        it "parses hl nick" $
            P.parseCommand "λcancer >  foo " `shouldParse` CoucouCmdCancer Nothing (Just "foo")
        it "parses search and hl nick" $
            P.parseCommand "λcancer  search  >  nick " `shouldParse` CoucouCmdCancer (Just "search") (Just "nick")
        it "parses factoid with > at the end" $
            P.parseCommand "λcancer foo>" `shouldParse` CoucouCmdCancer (Just "foo>") Nothing
        it "parses multiple search term" $
            P.parseCommand "λcancer foo bar > baz" `shouldParse` CoucouCmdCancer (Just "foo bar") (Just "baz")
        it "parses multiple search terms with hl" $
            P.parseCommand "λcancer foo bar > nick" `shouldParse` CoucouCmdCancer (Just "foo bar") (Just "nick")
        it "parses search term with > inside" $
            P.parseCommand "λcancer foo>bar " `shouldParse` CoucouCmdCancer (Just "foo>bar") Nothing



    describe "factoids command parser" $ do

        describe "counters" $ do
            it "parses counter++" $ do
                P.parseCommand "λfoo++" `shouldParse` CoucouCmdFactoid "foo" IncFactoid
                P.parseCommand "λfoo++  " `shouldParse` CoucouCmdFactoid "foo" IncFactoid
            it "parses counter--" $ do
                P.parseCommand "λfoo--"  `shouldParse` CoucouCmdFactoid "foo" DecFactoid
                P.parseCommand "λfoo--  "  `shouldParse` CoucouCmdFactoid "foo" DecFactoid
            it "only parses one word" $ do
                P.parseCommand "λfoo--  bar"  `shouldParse` CoucouCmdNop
                P.parseCommand "λfoo++  bar" `shouldParse` CoucouCmdNop

        describe "getting factoids" $ do
            it "parse only one word" $
                parse P.factoid "" `shouldFailOn` "λfoo bar"
            it "fails on blacklisted names" $
                P.parseCommand "λseen" `shouldParse` CoucouCmdNop
            it "parses «weird» names" $ do
                let factoid = "γ_éï机器-"
                let expectedCommand = CoucouCmdFactoid factoid (GetFactoid Nothing)
                P.parseCommand ("λ" <> factoid) `shouldParse` expectedCommand
            it "can hl user with a factoid" $
                P.parseCommand "λfoo > bar" `shouldParse` CoucouCmdFactoid "foo" (GetFactoid (Just "bar"))
            it "get factoid starting with cancer" $
                P.parseCommand "λcancerd" `shouldParse` CoucouCmdFactoid "cancerd" (GetFactoid Nothing)
            it "get factoid with dash inside" $
                P.parseCommand "λfoo-bar" `shouldParse` CoucouCmdFactoid "foo-bar" (GetFactoid Nothing)


        describe "setting factoids" $ do
            it "can set factoid" $
                P.parseCommand "λfoo = 1" `shouldParse` CoucouCmdFactoid "foo" (SetFactoid "1")
            it "can reset factoid" $
                P.parseCommand "λfoo := 1" `shouldParse` CoucouCmdFactoid "foo" (ResetFactoid "1")
            it "can delete factoid" $ do
                P.parseCommand "λfoo :=" `shouldParse` CoucouCmdFactoid "foo" DeleteFactoid
                P.parseCommand "λfoo :=  " `shouldParse` CoucouCmdFactoid "foo" DeleteFactoid
            it "can augment factoid" $
                P.parseCommand "λfoo +=  1" `shouldParse` CoucouCmdFactoid "foo" (AugmentFactoid "1")

        describe "display factoids" $
            it "can parse simple search" $
                P.parseCommand "λsee  foo-bar " `shouldParse` CoucouCmdFactoid "foo-bar" SeeFactoids

        describe "search factoids" $ do
            it "can parse simple search" $
                P.parseCommand "λsearch  foo "
                    `shouldParse` CoucouCmdFactoid "foo" (SearchFactoids Nothing)
            it "can parse additional search term" $
                P.parseCommand "λsearch  foo  bar "
                    `shouldParse` CoucouCmdFactoid "foo" (SearchFactoids (Just "bar"))

    describe "increase coucou count" $ do
        it "picks up coucou at the beginning" $
            P.parseCommand "coucou" `shouldParse` CoucouCmdIncCoucou
        it "picks up coucou with spaces around" $ do
            P.parseCommand "foo  coucou  bar" `shouldParse` CoucouCmdIncCoucou
            P.parseCommand "  coucou" `shouldParse` CoucouCmdIncCoucou
        it "doesn't increase coucou count if not full word" $ do
            parse P.getCoucou "" `shouldFailOn` "foocoucou bar"
            parse P.getCoucou "" `shouldFailOn` "foo coucoubar"

    describe "last seen parser" $ do
        it "parses correct command with nick" $
            P.parseCommand "λseen foonick " `shouldParse` CoucouCmdLastSeen "foonick" Nothing
        it "parses correct command with nick and underscore" $
            P.parseCommand "λseen foo_nick " `shouldParse` CoucouCmdLastSeen "foo_nick" Nothing
        it "doesn't parse command if no nick follows" $
            P.parseCommand "λseen" `shouldParse` CoucouCmdNop
        it "parses hl nick" $
            P.parseCommand "λseen foo_nick  > bar " `shouldParse` CoucouCmdLastSeen "foo_nick" (Just "bar")
        it "doesn't parse if only hl nick" $
            P.parseCommand "λseen > foo" `shouldParse` CoucouCmdNop

    describe "version parser" $ do
        it "parses version command" $
            P.parseCommand "λversion  " `shouldParse` CoucouCmdVersion
        it "doesn't parse if anything follows" $
            P.parseCommand "λversion  foo" `shouldParse` CoucouCmdNop

    describe "tell message" $ do
        it "parses tell command" $
            P.parseCommand "λtell foo  message  " `shouldParse` CoucouCmdTell "foo" "message  " Nothing
        it "needs a message" $
            P.parseCommand "λtell foo  " `shouldParse` CoucouCmdNop
        it "parses seconds delay" $ do
            P.parseCommand "λtell foo  in 4 seconds message"
                `shouldParse` CoucouCmdTell "foo" "message" (Just 4)
            P.parseCommand "λtell foo in 4 second message"
                `shouldParse` CoucouCmdTell "foo" "message" (Just 4)
        it "parses minutes delay" $ do
            P.parseCommand "λtell foo  in 4 minutes message"
                `shouldParse` CoucouCmdTell "foo" "message" (Just (4 * 60))
            P.parseCommand "λtell foo in 4 minute message"
                `shouldParse` CoucouCmdTell "foo" "message" (Just (4 * 60))
        it "parses hours delay" $ do
            P.parseCommand "λtell foo  in 4 hours message"
                `shouldParse` CoucouCmdTell "foo" "message" (Just (4 * 3600))
            P.parseCommand "λtell foo in 4 hour message"
                `shouldParse` CoucouCmdTell "foo" "message" (Just (4 * 3600))
        it "parses minutes seconds delay" $
            P.parseCommand "λtell foo  in 2 minutes 4 seconds message"
                `shouldParse` CoucouCmdTell "foo" "message" (Just (2 * 60 + 4))
        it "parses hours seconds delay" $
            P.parseCommand "λtell foo  in 2 hours 4 seconds message"
                `shouldParse` CoucouCmdTell "foo" "message" (Just (2 * 3600 + 4))
        it "parses hours minutes delay" $
            P.parseCommand "λtell foo  in 2 hours 4 minutes message"
                `shouldParse` CoucouCmdTell "foo" "message" (Just (2 * 3600 + 4 * 60))
        it "parses hours minutes seconds delay" $
            P.parseCommand "λtell foo  in 2 hours 4 minutes 10 seconds message"
                `shouldParse` CoucouCmdTell "foo" "message" (Just (2 * 3600 + 4 * 60 + 10))
        it "doesn't parse delay even with nick starting with delay command" $
            P.parseCommand "λtell in your face" `shouldParse` CoucouCmdTell "in" "your face" Nothing

        -- TODO fix that
        -- it "negative delays are considered 0" $
        --     P.parseCommand "λtell foo in -10 seconds message"
        --         `shouldParse` CoucouCmdTell "foo" "message" (Just 0)

    describe "help command" $ do
        it "parses generic help function" $
            P.parseCommand "λhelp" `shouldParse` CoucouCmdHelp Nothing
        it "parses help for specific commands" $
            P.parseCommand "λhelp cancer" `shouldParse` CoucouCmdHelp (Just TypeCancer)

    describe "url parser in message" $ do
        it "parses simple http" $
            P.parseUrl "http://bar" `shouldBe` Just "http://bar"
        it "parses http with space before" $
            P.parseUrl "  http://bar" `shouldBe` Just "http://bar"
        it "parses simple https" $
            P.parseUrl "https://bar" `shouldBe` Just "https://bar"
        it "parses url inside message" $
            P.parseUrl "foo http://moo bar" `shouldBe` Just "http://moo"
        it "ignore invalid urls prefix" $
            P.parseUrl "foo http https://foo.bar.com" `shouldBe` Just "https://foo.bar.com"

    describe "url command" $ do
        it "parses single command word" $
            P.parseCommand "λurl " `shouldParse` CoucouCmdUrl
        it "doesn't parse if something is after" $
            P.parseCommand "λurl foo bar" `shouldParse` CoucouCmdNop

    describe "coucourank command" $
        it "parses coucourank command" $
            P.parseCommand "λcoucourank " `shouldParse` CoucouCmdCoucouRank

    describe "cryptorate command" $ do
        it "parses cryptorate command" $
            P.parseCommand "λcrypto btc" `shouldParse` CoucouCmdCryptoRate Bitcoin Nothing
        it "requires a crypto symbol" $
            P.parseCommand "λcrypto" `shouldParse` CoucouCmdFactoid "crypto" (GetFactoid Nothing)
        it "can hl a nick" $
            P.parseCommand "λcrypto btc > foo" `shouldParse` CoucouCmdCryptoRate Bitcoin (Just "foo")
        it "can parse eth" $
            P.parseCommand "λcrypto eth" `shouldParse` CoucouCmdCryptoRate Ethereum Nothing
        it "doesn't parse random coin" $
            P.parseCommand "λcrypto foo" `shouldParse` CoucouCmdNop

    describe "french revolutionary calendar" $ do
        it "parses the date command" $
            P.parseCommand "λdate" `shouldParse` CoucouCmdCalendar Nothing
        it "parses the date command with some spaces at the end" $
            P.parseCommand "λdate  " `shouldParse` CoucouCmdCalendar Nothing
        it "parses the date with a hl" $
            P.parseCommand "λdate  > foo " `shouldParse` CoucouCmdCalendar (Just "foo")
