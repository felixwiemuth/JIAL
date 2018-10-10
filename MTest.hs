module MTest where

import System.Environment

import qualified Lexer as Lexer
import qualified Parser as Parser


main = do
  putStrLn "Command is \"p\" - available commands: l, p, lp, lf"
  loop "p"

loop cmd = do
  line <- getLine
  case line of
    "" -> return ()
    "l" -> do
      putStrLn "Switched to lexer"
      loop "l"
    "lf" -> do
      putStrLn "Switched to lexer (files)"
      loop "lf"
    "p" -> do
      putStrLn "Switched to parser"
      loop "p"
    "lp" -> do
      putStrLn "Switched to lexer and parser"
      loop "p"
    _ ->
      do
      -- let cmd = head $ words line
      -- let t = dropWhile (/=' ') line
      -- putStrLn $ runCmd cmd t
      case cmd of
        "lf" -> do
                f <- readFile line
                let tokens = Lexer.scanner f
                putStrLn $ show tokens
                return ()
        _ -> putStrLn $ runCmd cmd line
      loop cmd

runCmd cmd arg = case cmd of
  "l" -> (either id show (Lexer.scanner arg))
  -- "l" -> either id id (either Left (Right $ show $ Parser.parse) (Lexer.scanner arg)) --(either id show (Lexer.scanner arg))
  "p" -> case Lexer.scanner arg of
           Left err -> err
           Right tokens -> show $ Parser.parse tokens--case Parser.parse tokens of
  "lp" -> runCmd "l" arg ++ "\n" ++ runCmd "p" arg
             -- Left err -> err
             -- Right p -> show p
  -- "l" -> either id show (either Left (Parser.parse) (Lexer.scanner arg)) --(either id show (Lexer.scanner arg))
