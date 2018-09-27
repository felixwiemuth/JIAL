module Main where

import qualified Lexer as Lexer


  
main = do
  line <- getLine
  if null line
    then return ()
    else do
      -- let cmd = head $ words line
      -- let t = dropWhile (/=' ') line
      -- putStrLn $ runCmd cmd t
      putStrLn $ runCmd "p" line
      main


runCmd cmd arg = case cmd of
  "p" -> (either id show (Lexer.scanner arg))
