module Frontend where

import qualified Lexer as Lexer
import Parser


makeALGFromFiles :: [String] -> IO String
makeALGFromFiles files =
  let fs = map (\f -> (f, readFile f)) files :: [(String, IO String)]
  in do tasks <- mapM (\(filename, content) -> do s <- content; return $ makeTask s) fs
        -- print tasks
        return $ show tasks


makeTask :: String -> Either String Task
makeTask s = do tokens <- Lexer.scanner s
                return $ parse tokens
