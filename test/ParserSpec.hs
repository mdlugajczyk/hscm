{-# OPTIONS_GHC -fno-warn-orphans #-}

module ParserSpec where

import Text.ParserCombinators.Parsec
import Hscm.Parser
import Test.Hspec

instance Eq ParseError where
    p1 == p2 = (errorPos p1) == (errorPos p2)

shouldParse :: String -> Expression -> Expectation
shouldParse str result = do
  parse parseExpr "" str `shouldBe` Right result
  parse parseExpr "" strWithWhitespace `shouldBe` Right result
      where strWithWhitespace = "   " ++ str ++ "  "

spec :: Spec
spec =  do
  describe "Hscm.Parser" $ do
    
    context "Parsing Scheme strings" $ do

      it "should parse simple string" $ do
        shouldParse "\"asdf\"" $ String "asdf"

      it "should parse string with double quotes inside" $ do
        shouldParse "\"asd\\\"f\"" $ String "asd\"f"

      it "should parse string with escaped backslash" $ do
        shouldParse "\"\\\\fnord\"" $ String "\\fnord"

      it "should parse string with new line" $ do
        shouldParse "\"\\nfnord\"" $ String "\nfnord"
        shouldParse "\"\\n\\rfnord\"" $ String "\n\rfnord"
      it "should parse string tab" $ do
        shouldParse "\"fno\\trd\"" $ String "fno\trd"

    context "Parsing scheme characteres" $ do

      it "should parse simple character" $ do
        shouldParse"#\\a" $ Character 'a'
        shouldParse"#\\n" $ Character 'n'

      it "should parse special characters" $ do
        shouldParse "#\\space" $ Character ' '
        shouldParse "#\\ " $ Character ' '
        shouldParse "#\\newline" $ Character '\n'

    context "Parsing atoms" $ do

      it "should parse a symbol" $ do
        shouldParse "asdf" $ Symbol "asdf"

    context "Parsing numbers" $ do

      it "should parse an integer" $ do
        shouldParse "123" $ Number 123

      it "should parse negative integer" $ do
        shouldParse "-3" $ Number $ -3

    context "Parsing quoted expressions" $ do

      it "should parse quoted symbol" $ do
        shouldParse "'fnord" $ List [Symbol "quote", Symbol "fnord"]

      it "should parse quoted list" $ do
        shouldParse "'(foo bar buz)" $ List [Symbol "quote",
                                             List [Symbol "foo",
                                                   Symbol "bar",
                                                   Symbol "buz"]]

    context "Parsing lists" $ do

      it "should parse nested list" $ do
        shouldParse "(1 (foo bar) 3)" $ List [Number 1,
                                              (List [Symbol "foo",
                                                     Symbol "bar"]),
                                              Number 3]

      it "should parse dotted pair" $ do
        shouldParse "((1 2 3) . 4)" (DottedPair [h] t)
            where h = List [Number 1, Number 2, Number 3]
                  t = Number 4
