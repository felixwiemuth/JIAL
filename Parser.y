{
module Parser where
import qualified Lexer as L
import qualified Symbols as S
}

%name parse
%tokentype { L.Token }
%error { parseError }

%token
normalChar { L.NormalChar $$ }
stmntSep { L.StmntSep }
reply { L.Reply }
beginString { L.BeginString }
endString { L.EndString }
beginBlock { L.BeginBlock $$ }
endBlock { L.EndBlock $$ }
stringChar { L.StringChar $$ }
beginInput { L.BeginInput }
beginParamList { L.BeginParamList }
paramSep { L.ParamSep }
endParamList { L.EndParamList }
beginWhen { L.BeginWhen }
sp { L.Space $$ }
id { L.Id $$ }


%%

Task :: { [TaskElem] }
Task : TaskElemList { reverse $1 }

TaskElemList : {- empty -} { [] }
             | TaskElemList TaskElem { $2:$1  }

TaskElem : beginString StringCharList endString { TString $ reverse $2 }
         | NormalCharList { NormalCharBlock $1 }
         | stmntSep { NormalCharBlock S.stmntSep } -- TODO: in normal mode, do not have to consider ";" ? (only inside iaps); can use it as normal char block here (use Symbols.Sep as char)
         | IAP { $1 }

StringCharList :: { String }
StringCharList : {- empty -} { [] }
               | StringCharList stringChar { $2:$1 }

NormalCharList :: { String }
NormalCharList : NormalCharList_ { reverse $1 }

NormalCharList_ :: { String }
NormalCharList_ : normalChar { [$1] }
                | NormalCharList_ normalChar { $2:$1 }

IAP :: { TaskElem }
IAP : beginInput Input Action { IAP $2 $3 }

-- Spaces are always handled as a postfix such that next unit starts without space (if possible)
Input :: { Input }
Input : Sp MsgSig Sp { let (t,p) = $2 in Input {msgT = t, params = p, cond = Nothing} }
      | Sp MsgSig Sp beginWhen Sp NormalCharList Sp { let (t,p) = $2 in Input {msgT = t, params = p, cond = Just $6} }

MsgSig :: { (String, [(String, String)]) } -- (msgTypeName, [(paramType, paramName)])
MsgSig : id Sp PParamList { ($1, $3) }

PParamList :: { [(String, String)] } -- [(paramType, paramName)]
PParamList : beginParamList Sp ParamList Sp endParamList { $3 } -- parenthesed ParamList

ParamList :: { [(String, String)] } -- [(paramType, paramName)]
ParamList : ParamList_ { reverse $1 }

ParamList_ :: { [(String, String)] } -- [(paramType, paramName)] (in reversed order)
ParamList_ : {- empty -} { [] }
           | Param { [$1] }
           | ParamList_ Sp paramSep Sp Param { $5:$1 }

Param :: { (String, String) }
Param : id Sp id Sp { ($1, $3) }

Action :: { [ActionElem] }
Action : beginBlock ActionElemList endBlock { reverse $2 }

ActionElemList :: { [ActionElem] } -- reversed list of action elements
ActionElemList : {- empty -} { [] }
               | ActionElemList ActionElem { $2:$1 }

ActionElem :: { ActionElem }
ActionElem : NormalCharList { CodeBlock $1 }
           | stmntSep { Sep }
           | reply Sp id Sp beginParamList NormalCharList stmntSep { Reply{sp1=$2, rmsgT=$3, sp2=$4, code=$6} }

Sp :: { String }
Sp : sp          { $1 }
   | {- empty -} { "" }

{
parseError :: [L.Token] -> a
parseError tokens = error $ "Parse error on tokens " ++ show tokens

data Task
  = TaskList [TaskElem]
  deriving (Eq, Show)

data TaskElem
  = NormalCharBlock String
  | TString String
  | StmntSep
  | IAP Input [ActionElem]
  deriving (Eq, Show)

data Input = Input { msgT :: String
                   , params :: [(String, String)]
                   , cond :: Maybe String
                   }
  deriving (Eq, Show)

-- data Action = Action [ActionElem]
--   deriving (Eq, Show)

data ActionElem
  = CodeBlock String
  | Sep
  | Reply {sp1 :: String, rmsgT :: String, sp2 :: String, code :: String} -- code does not include beginParamList and stmntSep symbols
  | Send
  deriving (Eq, Show)

}
