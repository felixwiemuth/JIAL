module CodeGenerator where

import Data.List
import Data.Maybe

import Parser
import qualified MsgTypeLexer as L


-- Generate the message type definition class M from a given message type definition

mtfBegin = "public class M {"
mtfEnd = "\n}"
mtfClassBegin1 = "\n\n    public static class "
mtfClassBegin2 = " extends Message {"
mtfClassEnd = "\n    }"

pn = "p"
mtfClassParam = "\n        public final "

mtfCtorBegin = "\n\n        public "
mtfCtorEnd =   "\n        }"

mtfCtorParam = "\n            this."


-- Before the first "MsgTpyeName" it puts the "class" part
makeMsgTypeFile :: [L.Token] -> String
makeMsgTypeFile tokens = streamInitial tokens ""

streamInitial inp res =
  case inp of
    [] -> res ++ mtfBegin ++ mtfEnd
    (L.NormalCharBlock b):inp' -> streamInitial inp' (res ++ b)
    (L.MsgTypeName _):_ -> res ++ mtfBegin ++ (stream inp "" 0 "" "") ++ mtfEnd

-- paramCnt: number of next parameter
stream :: [L.Token] -> String -> Int -> String -> String -> String
stream inp res paramCnt ctorHead ctorBody =
  case inp of
    [] -> res ++ completeCurrentClass paramCnt ctorHead ctorBody
    (L.NormalCharBlock b):inp' -> stream inp' (res ++ b) paramCnt ctorHead ctorBody
    (L.MsgTypeName name):inp' -> stream inp' (res ++ completeCurrentClass paramCnt ctorHead ctorBody ++ mtfClassBegin1 ++ name ++ mtfClassBegin2) 1 (mtfCtorBegin ++ name ++ "(") ""
    (L.Id pType):inp' -> stream inp' (res ++ mtfClassParam ++ pType ++ " " ++ pname ++ ";") (paramCnt+1) ctorHead' ctorBody'
      where
        pname = pn ++ (show paramCnt)
        ctorHead' = ctorHead ++ pType ++ " " ++ pname ++ ", "
        ctorBody' = ctorBody ++ mtfCtorParam ++ pname ++ " = " ++ pname ++ ";"
    L.EOF:_ -> error "Unexpected token"

completeCurrentClass paramCnt ctorHead ctorBody =
  if paramCnt == 0 -- no last message type definition
  then ""
  else ctorHead' ++ ") {" ++ ctorBody ++ mtfCtorEnd ++ mtfClassEnd
    where ctorHead' =
            if paramCnt == 1
            then ctorHead
            else take (length ctorHead - 2) ctorHead -- Remove trailing ", "

-------------------------------------------------
-- Extract all message types from a list of tasks
-------------------------------------------------
getMsgTypesFromTasks :: [Task] -> [String]
getMsgTypesFromTasks ts = nub $ concat $ map getMsgTypesFromTask ts


getMsgTypesFromTask :: Task -> [String]
getMsgTypesFromTask t = nub $ concat $ map getMsgTypesFromIAP (elements t)

getMsgTypesFromIAP :: TaskElem -> [String]
getMsgTypesFromIAP e =
  case e of
    IAP inp elems ->
      (msgT inp):(catMaybes $ map getMsgTypeFromActionElem elems)
    _ -> []

getMsgTypeFromActionElem :: ActionElem -> Maybe String
getMsgTypeFromActionElem e =
  case e of
    Reply {rmsgT=t} -> Just t
    Send {smsgT=t} -> Just t
    _ -> Nothing


-- Creates Java source for a class containing all message type classes
