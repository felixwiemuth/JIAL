module AnalysisTest where

import Test.HUnit

import Analysis
import Parser


-- Helper functions to generate test IAPs
iap :: String -> [String] -> TaskElem
iap inputType sendTypes = IAP (Input {msgT = inputType, params = [], cond = Nothing})
                                (map (\sendType -> Send {sp1 = "", smsgT = sendType, sp2 = "", paramCode = "", toCode = ""}) sendTypes)

-- Test tasks
t = Task { prelude = "", name = "", elements = [], epilogue = "" }
t1 = t { name="1", elements=[iap "a" ["b", "c"]] }


-- "IAP" test: test correct extraction of IAP information from a list of tasks
it :: String -> [Task] -> [(GIAP, [String])] -> Test
it name tasks expectedResult = TestCase $ assertEqual name expectedResult result
      where result = getGIAPsFromTasks tasks

main = runTestTT testlist

testlist :: Test
testlist = TestList [
  it "T1" [t1] [(GIAP{task="1", gmsgT="a", gcond=Nothing}, ["b", "c"])]
                    ]
