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
beginTaskHeader { L.BeginTaskHeader }
beginTaskBody { L.BeginTaskBody }
endTask { L.EndTask }
stmntSep { L.StmntSep }
reply { L.Reply }
send { L.Send }
to { L.To }
beginString { L.BeginString }
endString { L.EndString }
-- beginBlock { L.BeginBlock $$ }
-- endBlock { L.EndBlock $$ }
-- stringChar { L.StringChar $$ }
beginInput { L.BeginInput }
beginParamList { L.BeginParamList }
paramSep { L.ParamSep }
endParamList { L.EndParamList }
beginWhen { L.BeginWhen }
beginAction { L.BeginAction }
endAction { L.EndAction }
sp { L.Space $$ }
id { L.Id $$ }


%%

Task :: { Task }
Task : Prelude beginTaskHeader Sp id Sp beginTaskBody TaskElemList endTask MaybeNormalCharList { Task { prelude = $1, name = $4, elements = reverse $7, epilogue = $9 } }
     | beginTaskHeader Sp id Sp beginTaskBody TaskElemList endTask MaybeNormalCharList { Task { prelude = "", name = $3, elements = reverse $6, epilogue = $8 } } -- TODO make just "MaybePrelude"?

TaskElemList : {- empty -} { [] }
             | TaskElemList TaskElem { $2:$1  }

TaskElem : -- beginString StringCharList endString { TString $ reverse $2 }
           NormalCharList { NormalCharBlock $1 }
         | stmntSep { NormalCharBlock S.stmntSep } -- in normal mode, ";" does not have to be considered specially, it is thus put into a NormalCharBlock
         | IAP { $1 }

-- StringCharList :: { String }
-- StringCharList : {- empty -} { [] }
--                | StringCharList stringChar { $2:$1 }

Prelude :: { String }
Prelude : Prelude_ { reverse $1 }

Prelude_ :: { String }
Prelude_ : normalChar { [$1] }
         | stmntSep   { [';'] }
         | Prelude_ normalChar { $2:$1 }
         | Prelude_ stmntSep { ';':$1 }

NormalCharList :: { String }
NormalCharList : NormalCharList_ { reverse $1 }

NormalCharList_ :: { String }
NormalCharList_ : normalChar { [$1] }
                -- | beginString StringCharList endString {  } -- If want to deal with Strings here
                | NormalCharList_ normalChar { $2:$1 }

MaybeNormalCharList :: { String }
MaybeNormalCharList : {- empty -} { "" }
                    | NormalCharList { $1 }

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
Action : beginAction ActionElemList endAction { reverse $2 } -- TODO block must be level 1

ActionElemList :: { [ActionElem] } -- reversed list of action elements
ActionElemList : {- empty -} { [] }
               | ActionElemList ActionElem { $2:$1 }

ActionElem :: { ActionElem }
ActionElem : NormalCharList { CodeBlock $1 }
           | stmntSep { Sep }
           | reply Sp id Sp beginParamList NormalCharList stmntSep { Reply{sp1=$2, rmsgT=$3, sp2=$4, paramCode=$6} }
           | send Sp id Sp beginParamList NormalCharList to NormalCharList stmntSep { Send{sp1=$2, smsgT=$3, sp2=$4, paramCode=$6, toCode=$8} }

Sp :: { String }
Sp : sp          { $1 }
   | {- empty -} { "" }

{
parseError :: [L.Token] -> a
parseError tokens = error $ "Parse error on tokens " ++ show tokens

data Task = Task { prelude :: String, name :: String, elements :: [TaskElem], epilogue :: String }
  deriving (Eq, Show)

data TaskElem
  = NormalCharBlock String
  -- | TString String
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
  | Reply {sp1 :: String, rmsgT :: String, sp2 :: String, paramCode :: String} -- code does not include beginParamList and stmntSep symbols
  | Send {sp1 :: String, smsgT :: String, sp2 :: String, paramCode :: String, toCode :: String}
  deriving (Eq, Show)

}
