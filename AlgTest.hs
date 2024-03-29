-- Tests with algorithms from files

module AlgTest where

import Test.HUnit

import Analysis
import AnalysisTest (st)
import Frontend


-- Paramters: Task name, amount to create of that task, a list of boolean Java expressions in the scope of the task to be checked after termination (at all instances of this task)
type TaskTestConfig = (String, Int, [String])

-- Result test: test the tasks' state after termination of the algorithm (not implemented yet)
rt :: String -> [TaskTestConfig] -> IO Test
rt name configs =
  -- let includedTasks
      -- taskConfigs = map (\name amount _ _ -> (name, amount)) cs
      -- javaAssertions = map (\name)
  -- TestList $ map mkTestFromTaskConfig configs
  return $ TestList []

-- mkTestFromTaskConfig :: TaskConfig -> Test
-- mkTestFromTaskConfig (name, amount, javaAssertions, cycles) =
--   let tasks


-- Cyclicity test: test the cycles in an algorithm used by the termination analysis (it suffices to provide each task name once)
-- 1st parameter: test name
-- 2nd parameter: file names of tasks to add to algorithm
-- 3nd parameter: list of expected cycles (format: "taskA.inputx, taskB.inputy(x==4)" where in parentheses the exact when-clause follows (excluding preceding and trailing whitespace))
ct :: String -> [String] -> [String] -> IO Test
ct name files expectedCycles =
  do tasks <- makeALGFromFiles (map (\f -> "alg/"++f++".jial") files) -- in IO monad
     return $ case tasks of
       Left err -> TestCase $ assertEqual "Parsing" True False -- NOTE: will not occur as long as program crashes when parse errors occur
       Right ts -> st name ts expectedCycles



main = do tests <- sequence testlist
          runTestTT (TestList tests)

testlist :: [IO Test]
testlist =
  [
    ct "1" ["A"] []
  , rt "1" [("A", 1, [])]
  , ct "2" ["A", "B"] ["A.a, B.b"]
  , rt "2" [("A", 1, []), ("B", 1, [])]
  , ct "2PC" ["2pcC", "2pcP"] []
  , rt "2PC" [("2pcC", 1, ["result == true"]), ("2pcP", 3, ["committed == true"])]
  -- Fibonacci by Anton Pirogov
  , ct "Fib" ["fibC","fibP"] ["C.result, P.iterate"]
  , rt "Fib" [("fibC", 1, ["n == 0", "res == 2"]), ("fibP", 1, ["a == 2"])]
  -- Collatz by Anton Pirogov
  , ct "Col" ["colI","colE","colO"] ["Even.result, Odd.result", "Even.perform, Odd.perform"]
  , rt "Col" [("colI", 1, ["steps == 111"]), ("colE", 1, []), ("colO", 1, [])]
  ]
