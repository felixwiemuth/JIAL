module Main where

import System.Environment

import Frontend

main = do args <- getArgs
          tasksString <- makeALGFromFiles args
          print tasksString
