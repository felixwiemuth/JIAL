module AnalysisTest where

import Test.HUnit

import Analysis
import Parser


-----------------------------------------
-- Helper functions to generate test IAPs
-----------------------------------------

-- Generate an IAP with the given input message type, sending the given message types
iap :: String -> [String] -> TaskElem
iap inputType sendTypes = IAP (Input {msgT = inputType, params = [], cond = Nothing})
                                (map (\sendType -> Send {sp1 = "", smsgT = sendType, sp2 = "", paramCode = "", toCode = ""}) sendTypes)

-- Test tasks
t = Task { prelude = "", name = "", elements = [], epilogue = "" }
t1 = t { name="A", elements=[iap "a" ["b", "c"]] }
t2 = t { name="B", elements=[iap "b" ["c", "d"]] }
t3 = t { name="C", elements=[iap "c" ["a", "b"]] }


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
    it "T1" [t1] [(GIAP{task="A", gmsgT="a", gcond=Nothing}, ["b", "c"])]
  , st "S1" [] []
  , st "S2a" [t1] []
  , st "S2b" [t2] []
  , st "S3a" [t1, t1] []
  , st "S4a" [t1, t2] []
  , st "S4b" [t1, t3] ["A.a, C.c"]
  , st "S4c" [t1, t2, t3] ["A.a, B.b, C.c"]
  ]


-- SCCs work!

tasks = [t1, t2]

edges = [(giap, giap, sendsTo) |
          (giap, canSendMsgTs) <- getGIAPsFromTasks tasks,
          let sendsTo = [giap' | (giap', _) <- getGIAPsFromTasks tasks, (gmsgT giap') `elem` canSendMsgTs] ]

