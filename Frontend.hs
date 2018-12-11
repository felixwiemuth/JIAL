module Frontend where

import Data.Either

import qualified Lexer as Lexer
import qualified MsgTypeLexer as MsgTypeLexer
import Parser
import Analysis
import qualified CodeGenerator as CG


makeALGFromFiles :: [String] -> IO (Either String [Task])
makeALGFromFiles files =
  let fs = map (\f -> (f, readFile f)) files :: [(String, IO String)]
  in do tasks <- mapM (\(filename, content) -> do s <- content; return $ makeTask s) fs
        if lefts tasks == []
          then return $ Right $ rights tasks
          else return $ Left "Error" -- TODO zip with task names, remove non-errors...


-- Take a list of task files and compile them to the given path (must end with /)
compileFiles :: [String] -> String -> IO ()
compileFiles files path = do
  ts <- makeALGFromFiles files
  case ts of
    Left err -> putStrLn $ "Error: " ++ err
    Right tasks ->
      let res = CG.makeTaskFiles tasks
      in do
        mapM (\(filename, content) -> writeFile (path ++ filename) content) res
        return ()
-- Take a message type file and compile it to the Java message type class M.java
compileMsgTypeFile :: String -> String -> IO ()
compileMsgTypeFile inFile path = do
  f <- readFile inFile
  let tokens = MsgTypeLexer.scanner f
  case tokens of
    Left err -> putStrLn err
    Right ts -> writeFile (path ++ "M.java") $ CG.makeMsgTypeFile ts
  return ()

compileMsgTypeFileAndTasks :: [String] -> String -> IO ()
compileMsgTypeFileAndTasks files path =
  let msgTypeFile = head files
      taskFiles = tail files
  in do
    compileMsgTypeFile msgTypeFile path
    compileFiles taskFiles path

showCyclesFromTaskFiles :: [String] -> IO (Either String String)
showCyclesFromTaskFiles files =
  do tasks <- makeALGFromFiles files -- in IO monad
     return $ do ts <- tasks -- in Either monad
                 return $ showCyclicSCCs ts


makeTask :: String -> Either String Task
makeTask s = do tokens <- Lexer.scanner s
                return $ parse tokens
