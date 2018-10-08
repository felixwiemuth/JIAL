module ParserTest where

import qualified Lexer as L
import Parser
import qualified LexerTest as L
import qualified Symbols as S
import Test.HUnit


-- mkParseTestCase :: [Token] -> Either String Task -> (Task -> Assertion) -> Test
-- mkParseTestCase name res mkA = TestCase $ if (assertFailure . ((name ++ ": ") ++)) mkA res

-- test lex
tp :: String -> [L.Token] -> [TaskElem] -> Test
tp name input expectedResult =
    TestCase $ assertEqual name expectedResult (parse input)


-- input constructors
-- Add a line comment with end of line
cmt :: String -> String
cmt s = "//" ++ s ++ "\n"

-- Token list constructors

-- String
mkStr = L.mkStr

-- Sequence of "normal characters" (no control characters)
mkN :: String -> [L.Token]
mkN = map L.NormalChar

sep = NormalCharBlock S.stmntSep


main = runTestTT testlist


-- Test list shortcuts

input1 = [L.BeginInput, L.sp, L.Id "A", L.BeginParamList, L.Id "int", L.sp, L.Id "i", L.EndParamList, L.sp]

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
  -- , tp "Sp1" [Space " "] " "
  , tp "Sep1" [L.StmntSep] [sep]
  , tp "Sep2" [L.StmntSep, L.StmntSep] [sep, sep]
  , tp "Iap1a" ([L.BeginInput, L.sp, L.Id "A", L.BeginParamList, L.Id "int", L.sp, L.Id "i", L.EndParamList, L.sp, L.BeginBlock 0, L.EndBlock 0]) $ [IAP (Input {msgT="A", params=[("int", "i")], cond=Nothing}) []]
  , tp "Iap1b" ([L.BeginInput, L.sp, L.Id "A", L.BeginParamList, L.Id "int", L.sp, L.Id "i", L.ParamSep, L.Id "String", L.sp, L.Id "s", L.EndParamList, L.BeginBlock 0, L.EndBlock 0]) $ [IAP (Input {msgT="A", params=[("int", "i"), ("String", "s")], cond=Nothing}) []]
  , tp "Iap3a" (input1 ++ [L.BeginBlock 0, L.StmntSep, L.EndBlock 0]) $ [IAP (Input {msgT="A", params=[("int", "i")], cond=Nothing}) [Sep]]
  , tp "Iap3b" (input1 ++ [L.BeginBlock 0] ++ mkN "int k=0" ++ [L.StmntSep, L.EndBlock 0]) $ [IAP (Input {msgT="A", params=[("int", "i")], cond=Nothing}) [CodeBlock "int k=0", Sep]]
  , tp "Iap3c" (input1 ++ [L.BeginBlock 0] ++ mkN "int k=0" ++ [L.StmntSep] ++ mkN "k++" ++ [L.StmntSep, L.EndBlock 0]) $ [IAP (Input {msgT="A", params=[("int", "i")], cond=Nothing}) [CodeBlock "int k=0", Sep, CodeBlock "k++", Sep]]
  , tp "Iap3d" (input1 ++ [L.BeginBlock 0] ++ mkN "int k=0" ++ [L.StmntSep, L.Reply, L.sp, L.Id "B", L.BeginParamList] ++ mkN "i)" ++ [L.StmntSep, L.EndBlock 0]) $ [IAP (Input {msgT="A", params=[("int", "i")], cond=Nothing}) [CodeBlock "int k=0", Sep, Reply{sp1=" ", rmsgT="B", sp2="", paramCode="i)"}]]
  , tp "Iap3e" (input1 ++ [L.BeginBlock 0] ++ mkN "int k=0" ++ [L.StmntSep, L.Send, L.sp, L.Id "B", L.BeginParamList] ++ mkN "i) " ++ [L.To] ++ mkN " 5" ++ [L.StmntSep, L.EndBlock 0]) $ [IAP (Input {msgT="A", params=[("int", "i")], cond=Nothing}) [CodeBlock "int k=0", Sep, Send{sp1=" ", smsgT="B", sp2="", paramCode="i) ", toCode=" 5"}]]
  , tp "Iap3f" (input1 ++ [L.BeginBlock 0] ++ mkN "int k=0" ++ [L.StmntSep, L.Send, L.sp, L.Id "X", L.BeginParamList] ++ mkN "i, (1+1)) " ++ [L.To] ++ mkN " getId(k) + 1" ++ [L.StmntSep, L.EndBlock 0]) $ [IAP (Input {msgT="A", params=[("int", "i")], cond=Nothing}) [CodeBlock "int k=0", Sep, Send{sp1=" ", smsgT="X", sp2="", paramCode="i, (1+1)) ", toCode=" getId(k) + 1"}]]
  , tp "Iap3g" (input1 ++ [L.BeginBlock 0] ++ mkN "int k=0" ++ [L.StmntSep, L.Send, L.sp, L.Id "B", L.BeginParamList] ++ mkN "i) " ++ [L.To] ++ mkN " 5" ++ [L.StmntSep, L.Reply, L.sp, L.Id "B", L.BeginParamList] ++ mkN "i)" ++ [L.StmntSep, L.EndBlock 0]) $ [IAP (Input {msgT="A", params=[("int", "i")], cond=Nothing}) [CodeBlock "int k=0", Sep, Send{sp1=" ", smsgT="B", sp2="", paramCode="i) ", toCode=" 5"}, Reply{sp1=" ", rmsgT="B", sp2="", paramCode="i)"}]]
  ]
