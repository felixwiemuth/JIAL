module Main where

import System.Environment

import Frontend

main = do args <- getArgs
          tasksString <- makeALGFromFiles args
          print tasksString
          cyclesString <- showCyclesFromTaskFiles args
          either (print . ("Error: " ++)) (print . ("Cycles: " ++)) cyclesString
