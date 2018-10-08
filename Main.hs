module Main where

import System.Environment

import qualified Lexer as Lexer
import qualified Parser as Parser


main = do args <- getArgs
          net <- makeALGFromFiles args
          print ""

makeALGFromFiles :: [String] -> IO String
makeALGFromFiles files =
  let fs = map (\f -> (f, readFile f)) files :: [(String, IO String)]
  in do return ""
