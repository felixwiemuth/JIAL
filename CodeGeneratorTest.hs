module CodeGeneratorTest where

import Test.HUnit

import CodeGenerator
import Parser
import TestUtil



-- Test tasks
t = Task { prelude = "", name = "", elements = [], epilogue = "" }
tA = t { name="A", elements=[iap "a" ["b", "c"]] }
tB = t { name="B", elements=[iap "b" ["c", "d"]] }
tC = t { name="C", elements=[iap "c" ["a", "b"]] }
tD = t { name="D", elements=[iap "d" ["e1", "e2"]] }
tE = t { name="E", elements=[iap "e1" ["a"], iap "e2" ["c", "d"]] }
tS = t { name="S", elements=[iap "self" ["self"]] }
-- tW = t { name="W", elements=[IAP (Input {msgT = "q", params=[("int", "a")], cond = Just "  a == 3\n"}) [Send{smsgT="q", paramCode="", toCode=""}]]}



-- Test extracting message types from tasks
mt :: String -> [Task] -> [String] -> Test
mt name tasks expectedResult = TestCase $ assertEqual name expectedResult result
  where result = getMsgTypesFromTasks tasks

main = runTestTT testlist

testlist :: Test
testlist = TestList
  [
    mt "M0" [] []
  , mt "MA" [tA] ["a", "b", "c"]
  , mt "MB" [tB] ["b", "c", "d"]
  , mt "MC" [tC] ["c", "a", "b"]
  , mt "ME" [tE] ["e1", "a", "e2", "c", "d"]
  , mt "MS" [tS] ["self"]
  , mt "M1" [tA, tB] ["a", "b", "c", "d"]
  , mt "M2" [tA, tB, tC] ["a", "b", "c", "d"]
  ]
