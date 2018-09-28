module Main where

import qualified Lexer as Lexer
import qualified Parser as Parser


main = loop "l"

loop cmd = do
  line <- getLine
  case line of
    "" -> return ()
    "l" -> loop "l"
    "p" -> loop "p"
    _ ->
      do
      -- let cmd = head $ words line
      -- let t = dropWhile (/=' ') line
      -- putStrLn $ runCmd cmd t
      putStrLn $ runCmd cmd line
      loop cmd

runCmd cmd arg = case cmd of
  "p" -> (either id show (Lexer.scanner arg))
  -- "l" -> either id id (either Left (Right $ show $ Parser.parse) (Lexer.scanner arg)) --(either id show (Lexer.scanner arg))
  "l" -> case Lexer.scanner arg of
           Left err -> err
           Right tokens -> show $ Parser.parse tokens--case Parser.parse tokens of
             -- Left err -> err
             -- Right p -> show p
  -- "l" -> either id show (either Left (Parser.parse) (Lexer.scanner arg)) --(either id show (Lexer.scanner arg))
