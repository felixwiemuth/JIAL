module Main where

import System.Environment

import Frontend

main = do args <- getArgs
          compileMsgTypeFileAndTasks args "./"
