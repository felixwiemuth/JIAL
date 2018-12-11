module Main where

import System.Environment

import Frontend

main = do args <- getArgs
          cyclesString <- showCyclesFromTaskFiles args
          either (print . ("Error: " ++)) (print . ("Cycles: " ++)) cyclesString
