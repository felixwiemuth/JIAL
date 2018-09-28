{
  module Parser (Task, TaskElem(..), parse) where
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

Task :: { [TaskElem] }
Task : TaskList { reverse $1 }

TaskList : {- empty -} { [] }
  | TaskList TaskElem { $2:$1  }

TaskElem : beginString StringCharList endString { TString $ reverse $2 }
         | NormalCharList { NormalCharBlock $ reverse $1 }

StringCharList :: { String }
StringCharList : {- empty -} { [] }
               | StringCharList stringChar { $2:$1 }


NormalCharList :: { String }
NormalCharList : normalChar { [$1] }
               | NormalCharList normalChar { $2:$1 }


{
parseError :: [L.Token] -> a
parseError tokens = error $ "Parse error on tokens " ++ show tokens

data Task
  = TaskList [TaskElem]
  deriving (Eq, Show)

data TaskElem
  = NormalCharBlock String
  | TString String
  deriving (Eq, Show)

}
