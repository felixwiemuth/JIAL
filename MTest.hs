module MTest where

import System.Environment

import qualified Lexer as Lexer
import qualified MsgTypeLexer as MsgTypeLexer
import qualified Parser as Parser
import qualified CodeGenerator as CG


main = do
  putStrLn "Command is \"p\" - available commands: l, p, lp, lf, m, mf, mc (f = read from file, c = compile file to file out)"
  loop "p"

loop cmd = do
  line <- getLine
  case line of
    "" -> return ()
    "l" -> do
      putStrLn "Switched to lexer"
      loop "l"
    "m" -> do
      putStrLn "Switched to message type lexer"
      loop "m"
    "lf" -> do
      putStrLn "Switched to lexer (files)"
      loop "lf"
    "mf" -> do
      putStrLn "Switched to message type file lexer"
      loop "mf"
    "mc" -> do
      putStrLn "Switched to message type file compiler"
      loop "mc"
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
        "mf" -> do
                f <- readFile line
                let tokens = MsgTypeLexer.scanner f
                putStrLn $ show tokens
                return ()
        "mc" -> do
                f <- readFile line
                let tokens = MsgTypeLexer.scanner f
                case tokens of
                  Left err -> putStrLn err
                  Right ts -> writeFile "M.java" $ CG.makeMsgTypeFile ts
                return ()
        _ -> putStrLn $ runCmd cmd line
      loop cmd

runCmd cmd arg = case cmd of
  "l" -> (either id show (Lexer.scanner arg))
  -- "l" -> either id id (either Left (Right $ show $ Parser.parse) (Lexer.scanner arg)) --(either id show (Lexer.scanner arg))
  "m" -> (either id show (MsgTypeLexer.scanner arg))
  "p" -> case Lexer.scanner arg of
           Left err -> err
           Right tokens -> show $ Parser.parse tokens--case Parser.parse tokens of
  "lp" -> runCmd "l" arg ++ "\n" ++ runCmd "p" arg
             -- Left err -> err
             -- Right p -> show p
  -- "l" -> either id show (either Left (Parser.parse) (Lexer.scanner arg)) --(either id show (Lexer.scanner arg))
