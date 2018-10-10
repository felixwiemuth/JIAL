module ParserTest where

import qualified Lexer as L
import Parser
import qualified LexerTest as L
import qualified Symbols as S
import Test.HUnit


-- mkParseTestCase :: [Token] -> Either String Task -> (Task -> Assertion) -> Test
-- mkParseTestCase name res mkA = TestCase $ if (assertFailure . ((name ++ ": ") ++)) mkA res

-- test parsing of task elements
tp :: String -> [L.Token] -> [TaskElem] -> Test
tp name input expectedResult =
    TestCase $ assertEqual name expectedResult result
    where result = elements (parse ([L.BeginTaskHeader, L.sp, L.Id "A", L.BeginTaskBody] ++ input ++ [L.EndTask]))

-- test whole file parsing
tfp :: String -> [L.Token] -> [TaskElem] -> Test
tfp name input expectedResult =
    TestCase $ assertEqual name expectedResult result
    where result = elements (parse ([L.BeginTaskHeader, L.sp, L.Id "A", L.BeginTaskBody] ++ input ++ [L.EndTask]))

-- input constructors
-- Add a line comment with end of line
cmt :: String -> String
cmt s = "//" ++ s ++ "\n"

-- Token list constructors

mkStr :: String -> TaskElem
mkStr s = NormalCharBlock $ "\"" ++ s ++ "\""

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
  -- NOTE: the lexer does not produce separate string symbols anymore
  -- , tp "S1" (L.mkStr "") $ [TString ""]
  -- , tp "S2" (L.mkStr "abc") $ [TString "abc"]
  -- , tp "S3" (L.mkStr " a b c") $ [TString " a b c"]
  , tp "AS1" (mkN "" ++ L.mkStr "") $ [mkStr ""]
  , tp "AS2" (mkN "a" ++ L.mkStr "") $ [NormalCharBlock "a\"\""]
  , tp "AS3" (mkN "a" ++ L.mkStr "a") $ [NormalCharBlock "a\"a\""]
  , tp "AS4" (mkN " ! q //p" ++ L.mkStr "/*sd*/") $ [NormalCharBlock " ! q //p\"/*sd*/\""]
  -- , tp "AS5" (L.mkStr "x x" ++ mkN "a" ++ L.mkStr "a") $ [TString "x x", NormalCharBlock "a", TString "a"]
  -- , tp "AS6" (L.mkStr "x x" ++ mkN "a" ++ L.mkStr "a" ++ mkN "") $ [TString "x x", NormalCharBlock "a", TString "a"]
  -- , tp "AS7" (L.mkStr "x x" ++ mkN "a" ++ L.mkStr "a" ++ mkN " ") $ [TString "x x", NormalCharBlock "a", TString "a", NormalCharBlock " "]
  -- , tp "Sp1" [Space " "] " "
  , tp "Sep1" [L.StmntSep] [sep]
  , tp "Sep2" [L.StmntSep, L.StmntSep] [sep, sep]
  , tp "Iap1a" ([L.BeginInput, L.sp, L.Id "A", L.BeginParamList, L.Id "int", L.sp, L.Id "i", L.EndParamList, L.sp, L.BeginAction, L.EndAction]) $ [IAP (Input {msgT="A", params=[("int", "i")], cond=Nothing}) []]
  , tp "Iap1b" ([L.BeginInput, L.sp, L.Id "A", L.BeginParamList, L.Id "int", L.sp, L.Id "i", L.ParamSep, L.Id "String", L.sp, L.Id "s", L.EndParamList, L.BeginAction, L.EndAction]) $ [IAP (Input {msgT="A", params=[("int", "i"), ("String", "s")], cond=Nothing}) []]
  , tp "Iap3a" (input1 ++ [L.BeginAction, L.StmntSep, L.EndAction]) $ [IAP (Input {msgT="A", params=[("int", "i")], cond=Nothing}) [Sep]]
  , tp "Iap3b" (input1 ++ [L.BeginAction] ++ mkN "int k=0" ++ [L.StmntSep, L.EndAction]) $ [IAP (Input {msgT="A", params=[("int", "i")], cond=Nothing}) [CodeBlock "int k=0", Sep]]
  , tp "Iap3c" (input1 ++ [L.BeginAction] ++ mkN "int k=0" ++ [L.StmntSep] ++ mkN "k++" ++ [L.StmntSep, L.EndAction]) $ [IAP (Input {msgT="A", params=[("int", "i")], cond=Nothing}) [CodeBlock "int k=0", Sep, CodeBlock "k++", Sep]]
  , tp "Iap3d" (input1 ++ [L.BeginAction] ++ mkN "int k=0" ++ [L.StmntSep, L.Reply, L.sp, L.Id "B", L.BeginParamList] ++ mkN "i)" ++ [L.StmntSep, L.EndAction]) $ [IAP (Input {msgT="A", params=[("int", "i")], cond=Nothing}) [CodeBlock "int k=0", Sep, Reply{sp1=" ", rmsgT="B", sp2="", paramCode="i)"}]]
  , tp "Iap3e" (input1 ++ [L.BeginAction] ++ mkN "int k=0" ++ [L.StmntSep, L.Send, L.sp, L.Id "B", L.BeginParamList] ++ mkN "i) " ++ [L.To] ++ mkN " 5" ++ [L.StmntSep, L.EndAction]) $ [IAP (Input {msgT="A", params=[("int", "i")], cond=Nothing}) [CodeBlock "int k=0", Sep, Send{sp1=" ", smsgT="B", sp2="", paramCode="i) ", toCode=" 5"}]]
  , tp "Iap3f" (input1 ++ [L.BeginAction] ++ mkN "int k=0" ++ [L.StmntSep, L.Send, L.sp, L.Id "X", L.BeginParamList] ++ mkN "i, (1+1)) " ++ [L.To] ++ mkN " getId(k) + 1" ++ [L.StmntSep, L.EndAction]) $ [IAP (Input {msgT="A", params=[("int", "i")], cond=Nothing}) [CodeBlock "int k=0", Sep, Send{sp1=" ", smsgT="X", sp2="", paramCode="i, (1+1)) ", toCode=" getId(k) + 1"}]]
  , tp "Iap3g" (input1 ++ [L.BeginAction] ++ mkN "int k=0" ++ [L.StmntSep, L.Send, L.sp, L.Id "B", L.BeginParamList] ++ mkN "i) " ++ [L.To] ++ mkN " 5" ++ [L.StmntSep, L.Reply, L.sp, L.Id "B", L.BeginParamList] ++ mkN "i)" ++ [L.StmntSep, L.EndAction]) $ [IAP (Input {msgT="A", params=[("int", "i")], cond=Nothing}) [CodeBlock "int k=0", Sep, Send{sp1=" ", smsgT="B", sp2="", paramCode="i) ", toCode=" 5"}, Reply{sp1=" ", rmsgT="B", sp2="", paramCode="i)"}]]
  , tp "B1" (mkN "{List<String> myStrings" ++ [L.StmntSep] ++ mkN "\nmyStrings.add(\"s\")" ++ [L.StmntSep] ++ mkN "}") [NormalCharBlock "{List<String> myStrings", NormalCharBlock S.stmntSep, NormalCharBlock "\nmyStrings.add(\"s\")", NormalCharBlock S.stmntSep, NormalCharBlock "}"]
  ]
