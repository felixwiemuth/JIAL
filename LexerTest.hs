module LexerTest where

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


-- input constructors
-- Add a line comment with end of line
cmt :: String -> String
cmt s = "//" ++ s ++ "\n"

-- Token list constructors

-- String
-- mkStr :: String -> [Token]
-- mkStr s = [BeginString] ++ map StringChar s ++ [EndString]
mkStr :: String -> [Token]
mkStr s = NormalChar '"' : mkN s ++ [NormalChar '"']

-- Sequence of "normal characters" (no control characters)
mkN :: String -> [Token]
mkN = map NormalChar

-- Single space
sp = Space " "

main = runTestTT testlist

testlist :: Test
testlist = TestList [
  -- Basics
    tl "A1" "a" $ mkN "a"
  , tl "A2" "ab" $ mkN "ab"
  , tl "A3" "a b" $ mkN "a b"
  , tl "A4a" "(" $ mkN "("
  , tl "A4b" "$" $ mkN "$"
  , tl "A4c" "><,." $ mkN "><,."
  , tl "Sep1a" ";" $ [StmntSep]
  , tl "Sep1b" ";;" $ [StmntSep, StmntSep]
  , tl "Sep1c" " ; ;" $ mkN " " ++ [StmntSep] ++ mkN " " ++ [StmntSep]
  , tl "Sep2" "a;b" $ mkN "a" ++ [StmntSep] ++ mkN "b"
  -- NOTE: Strings are not specially lexed anymore
  -- , tl "S1" "\"" [BeginString]
  -- , tl "S2" "\"ok" [BeginString, StringChar 'o', StringChar 'k']
  -- , tl "S3" "\"ok\"" [BeginString, StringChar 'o', StringChar 'k', EndString]
  -- , tl "S4" "\"ok\"" [BeginString, StringChar 'o', StringChar 'k', EndString]
  , tl "S5" "\"Hello World String\"" $ mkStr "Hello World String"
  , tl "S6" "/* commented \"String\" */" []
  , tl "S7" "/* commented \"String\" */ and \"string\" " $ mkN " and " ++ mkStr "string" ++ mkN " "
  , tl "C1" "// Line comment\n" []
  , tl "C2" " // Line comment\n" $ mkN " "
  , tl "C3" "sth., then: // Line comment\n" $ mkN "sth., then: "
  , tl "C4" "// Line comment with stuff inside /* dsd */ \" \" //\n" $ []
  , tl "B1a" "{" [BeginTaskBody]
  -- , tl "B1b" "}" [EndBlock (-1)]
  , tl "B2a" "{}" [BeginTaskBody, EndTask]
  , tl "B2b" "{{}" [BeginTaskBody, BeginAction, EndAction]
  , tl "B2c" "{{}}" [BeginTaskBody, BeginAction, EndAction, EndTask]
  -- , tl "B2d" "{{}}}" [BeginTaskBody, BeginAction, EndAction, EndTask, EndBlock (-1)]
  , tl "B3a" (cmt "{") []
  , tl "B3b" (cmt "}") []
  , tl "B3c" (cmt "{}") []
  -- Input-Action-pairs, reply/send
  , tl "When1" "input when true" $ [BeginInput, sp, BeginWhen] ++ mkN " true"
  , tl "Input1a" "input " [BeginInput, sp]
  , tl "Input1b" "input\n" $ [BeginInput, Space "\n"]
  , tl "Input1c" " input\n" $ [NormalChar ' ', BeginInput, Space "\n"]
  , tl "Input2a" "_input" $ mkN "_input"
  , tl "Input2b" "input_" $ mkN "input_"
  -- , tl "Input2c" " input\n" $ mkN " input\n"
  , tl "IapIn1" "input msgX(int x) {}" $ [BeginInput, sp, Id "msgX", BeginParamList, Id "int", sp, Id "x", EndParamList, sp, BeginTaskBody, EndTask]
  , tl "IapIn2" "input msgX(int x) when x > 0 {}" $ [BeginInput, sp, Id "msgX", BeginParamList, Id "int", sp, Id "x", EndParamList, sp, BeginWhen] ++ mkN " x > 0 " ++ [BeginTaskBody, EndTask]
  , tl "IapIn3" "input msgX(int x,  String myS) when x > 0 && s=\"s\" {}" $ [BeginInput, sp, Id "msgX", BeginParamList, Id "int", sp, Id "x", ParamSep, Space "  ", Id "String", sp, Id "myS", EndParamList, sp, BeginWhen] ++ mkN " x > 0 && s=\"s\" " ++ [BeginTaskBody, EndTask]
  , tl "Send1a" "send " [Send, sp]
  , tl "Send1b" "\nsend " [NormalChar '\n', Send, sp]
  , tl "Send1c" "\nsend\n" [NormalChar '\n', Send, Space "\n"]
  , tl "Send2a" " send" $ mkN " send"
  , tl "Send2b" "_send" $ mkN "_send"
  , tl "To1a" "to " [To, NormalChar ' ']
  , tl "To1b" "\nto " [NormalChar '\n', To, NormalChar ' ']
  , tl "To1c" "\nto\n" [NormalChar '\n', To, NormalChar '\n']
  , tl "To2a" " to" $ mkN " to"
  , tl "To2b" "_to" $ mkN "_to"
  , tl "SendTo1" "send msgX(0) to 4" $ [Send, sp, Id "msgX", BeginParamList] ++ mkN "0) " ++ [To] ++ mkN " 4"
  , tl "SendTo2" "send\n  My(getX(), \"send\") to getDest();" $ [Send, Space "\n  ", Id "My", BeginParamList] ++ mkN "getX(), " ++ mkStr "send" ++ mkN ") " ++ [To] ++ mkN " getDest()" ++ [StmntSep]
  , tl "Reply1a" "reply " [Reply, sp]
  , tl "Reply1b" "\nreply " [NormalChar '\n', Reply, sp]
  , tl "Reply1c" "\nreply\n" [NormalChar '\n', Reply, Space "\n"]
  , tl "Reply2a" " reply" $ mkN " reply"
  , tl "Reply2b" "_reply" $ mkN "_reply"
  , tl "Reply3a" "reply  m((1 + 2), a) ;" $ [Reply, Space "  ", Id "m", BeginParamList] ++ mkN "(1 + 2), a) " ++ [StmntSep]
  , tl "Reply3b" "reply  m ((1 + 2), a) ;" $ [Reply, Space "  ", Id "m", Space " ", BeginParamList] ++ mkN "(1 + 2), a) " ++ [StmntSep]
  , tl "Iap1" "input A(int i) {reply B(i);}" $ [BeginInput, sp, Id "A", BeginParamList, Id "int", sp, Id "i", EndParamList, sp, BeginTaskBody, Reply, sp, Id "B", BeginParamList] ++ mkN "i)" ++ [StmntSep, EndTask]
  -- File format
  , tl "F1" "task A {}" [BeginTaskHeader, sp, Id "A", sp, BeginTaskBody, EndTask]
  , tl "F2" "\ntask A {}" [NormalChar '\n', BeginTaskHeader, sp, Id "A", sp, BeginTaskBody, EndTask]
  , tl "F3" "task  A  { } " [BeginTaskHeader, Space "  ", Id "A", Space "  ", BeginTaskBody, NormalChar ' ', EndTask, NormalChar ' ']
  ]
