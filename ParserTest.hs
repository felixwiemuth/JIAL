module ParserTest where

import Lexer (Token(..), scanner)
import Parser (Task, TaskElem(..), parse)
import LexerTest
import Test.HUnit


-- mkParseTestCase :: [Token] -> Either String Task -> (Task -> Assertion) -> Test
-- mkParseTestCase name res mkA = TestCase $ if (assertFailure . ((name ++ ": ") ++)) mkA res

-- test lex
tp :: String -> [Token] -> [TaskElem] -> Test
tp name input expectedResult =
    TestCase $ assertEqual name expectedResult (parse input)


-- input constructors
-- Add a line comment with end of line
cmt :: String -> String
cmt s = "//" ++ s ++ "\n"

-- Token list constructors

-- String
mkStr :: String -> [Token]
mkStr s = [BeginString] ++ map StringChar s ++ [EndString]

-- Sequence of "normal characters" (no control characters)
mkN :: String -> [Token]
mkN = map NormalChar


main = runTestTT testlist

testlist :: Test
testlist = TestList [
    tp "A1" (mkN "") $ []
  , tp "A2a" (mkN "a") $ [NormalCharBlock "a"]
  , tp "A2b" (mkN "ab") $ [NormalCharBlock "ab"]
  , tp "S1" (mkStr "") $ [TString ""]
  , tp "S2" (mkStr "abc") $ [TString "abc"]
  , tp "S3" (mkStr " a b c") $ [TString " a b c"]
  , tp "AS1" (mkN "" ++ mkStr "") $ [TString ""]
  , tp "AS2" (mkN "a" ++ mkStr "") $ [NormalCharBlock "a", TString ""]
  , tp "AS3" (mkN "a" ++ mkStr "a") $ [NormalCharBlock "a", TString "a"]
  , tp "AS4" (mkN " ! q //p" ++ mkStr "/*sd*/") $ [NormalCharBlock " ! q //p", TString "/*sd*/"]
  , tp "AS5" (mkStr "x x" ++ mkN "a" ++ mkStr "a") $ [TString "x x", NormalCharBlock "a", TString "a"]
  , tp "AS6" (mkStr "x x" ++ mkN "a" ++ mkStr "a" ++ mkN "") $ [TString "x x", NormalCharBlock "a", TString "a"]
  , tp "AS7" (mkStr "x x" ++ mkN "a" ++ mkStr "a" ++ mkN " ") $ [TString "x x", NormalCharBlock "a", TString "a", NormalCharBlock " "]
  ]
