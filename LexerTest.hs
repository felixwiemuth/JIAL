module LexerTest (main) where

import Lexer (Token(..), scanner)
import Test.HUnit


-- mkA: sth. that takes a parameter and creates an assertion
-- res: actual result
mkLexTestCase :: String -> Either String [Token] -> ([Token] -> Assertion) -> Test
mkLexTestCase name res mkA = TestCase $ either (assertFailure . ((name ++ ": ") ++)) mkA res

-- test lex
tl :: String -> String -> [Token] -> Test
tl name input expectedResult =
  mkLexTestCase name (scanner input) (\res -> assertEqual name expectedResult res)


main = runTestTT testlist

testlist :: Test
testlist = TestList [
  tl "A1" "a" [ID "a"],
  tl "A2" "ab" [ID "ab"],
  tl "A3" "a b" [ID "a", ID "b"]
  ]
