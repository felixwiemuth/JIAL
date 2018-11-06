module AnalysisTest where

import Test.HUnit

import Analysis
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



-- "IAP" test: test correct extraction of IAP information from a list of tasks
it :: String -> [Task] -> [(GIAP, [String])] -> Test
it name tasks expectedResult = TestCase $ assertEqual name expectedResult result
  where result = getGIAPsFromTasks tasks

-- "SCC" test: test correct identification of strongly connected components (of size >1) in the msgtg
st :: String -> [Task] -> [String] -> Test
st name tasks expectedResult = TestCase $ assertEqual name expectedResult (map printGIAPList result)
  where result = getCyclicSCCs tasks

main = runTestTT testlist

testlist :: Test
testlist = TestList
  [
    it "T1" [tA] [(GIAP{task="A", gmsgT="a", gcond=Nothing}, ["b", "c"])]
  , st "S1" [] []
  , st "S2a" [tA] []
  , st "S2b" [tB] []
  , st "S3a" [tA, tA] []
  , st "S4a" [tA, tB] []
  , st "S4b" [tA, tC] ["A.a, C.c"]
  , st "S4c" [tB, tC] ["B.b, C.c"]
  , st "S4d" [tA, tB, tC] ["A.a, B.b, C.c"]
  , st "S4e" [tB, tC, tD, tE] ["B.b, C.c, D.d, E.e2"] -- todo want that one is not included
  , st "S5" [tS] ["S.self"]
  -- , st "S6" [tW] [""]
  ]


-- SCCs work!

tasks = [tA, tB]

edges = [(giap, giap, sendsTo) |
          (giap, canSendMsgTs) <- getGIAPsFromTasks tasks,
          let sendsTo = [giap' | (giap', _) <- getGIAPsFromTasks tasks, (gmsgT giap') `elem` canSendMsgTs] ]

