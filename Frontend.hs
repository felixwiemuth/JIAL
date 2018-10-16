module Frontend where

import Data.Either

import qualified Lexer as Lexer
import Parser
import Analysis


makeALGFromFiles :: [String] -> IO (Either String [Task])
makeALGFromFiles files =
  let fs = map (\f -> (f, readFile f)) files :: [(String, IO String)]
  in do tasks <- mapM (\(filename, content) -> do s <- content; return $ makeTask s) fs
        if lefts tasks == []
          then return $ Right $ rights tasks
          else return $ Left "Error" -- TODO zip with task names, remove non-errors...


showCyclesFromTaskFiles :: [String] -> IO (Either String String)
showCyclesFromTaskFiles files =
  do tasks <- makeALGFromFiles files -- in IO monad
     return $ do ts <- tasks -- in Either monad
                 return $ showCyclicSCCs ts


makeTask :: String -> Either String Task
makeTask s = do tokens <- Lexer.scanner s
                return $ parse tokens
