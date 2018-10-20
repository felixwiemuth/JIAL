-- Tests with algorithms from files

module AlgTest where

import Test.HUnit

import Analysis
import AnalysisTest (st)
import Frontend


-- Paramters: Task name, amount to create of that task, a list of boolean Java expressions in the scope of the task to be checked after termination (at all instances of this task)
type TaskTestConfig = (String, Int, [String])

-- Result test: test the tasks' state after termination of the algorithm
rt :: [TaskTestConfig] -> [String] -> IO Test
rt configs expectedCycles =
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
  do tasks <- makeALGFromFiles (map ("alg/"++) files) -- in IO monad
     return $ case tasks of
       Left err -> TestCase $ assertEqual "Parsing" True False -- NOTE: will not occur as long as program crashes because of parse errors
       Right ts -> st name ts expectedCycles



main = do tests <- sequence testlist
          runTestTT (TestList tests)

testlist :: [IO Test]
testlist =
  [
    ct "1" ["A"] []
  , ct "2" ["A", "B"] ["A.a, B.b"]
  ]
