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
    tl "A1" "a" $ mkN "a"
  , tl "A2" "ab" $ mkN "ab"
  , tl "A3" "a b" $ mkN "a b"
  , tl "S1" "\"" [BeginString]
  , tl "S2" "\"ok" [BeginString, StringChar 'o', StringChar 'k']
  , tl "S3" "\"ok\"" [BeginString, StringChar 'o', StringChar 'k', EndString]
  , tl "S4" "\"ok\"" [BeginString, StringChar 'o', StringChar 'k', EndString]
  , tl "S5" "\"Hello World String\"" $ mkStr "Hello World String"
  , tl "S6" "/* commented \"String\" */" []
  , tl "S7" "/* commented \"String\" */ and \"string\" " $ mkN " and " ++ mkStr "string" ++ mkN " "
  ]
