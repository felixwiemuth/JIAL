{
module Parser where
import qualified Lexer as L
}

%name parse
%tokentype { L.Token }
%error { parseError }

%token
normalChar { L.NormalChar $$ }
beginString { L.BeginString }
endString { L.EndString }
stringChar { L.StringChar $$ }


%%

Task : TaskList { reverse $1 }

TaskList : {- empty -} { [] }
  | TaskList TaskElem { $2:$1  }

TaskElem : beginString StringCharList endString { TString $ reverse $2 }

StringCharList :: { String }
StringCharList : {- empty -} { [] }
  | StringCharList stringChar { $2:$1 }

{
parseError :: [L.Token] -> a
parseError tokens = error $ "Parse error on tokens " ++ show tokens

data Task
  = TaskList [TaskElem]
  deriving (Show)

data TaskElem
  = NormalChar Char
  | TString String
  deriving (Show)

}
