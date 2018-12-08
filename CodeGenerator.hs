module CodeGenerator where

import Data.List
import Data.Maybe

import Parser
import qualified MsgTypeLexer as L


-- Generate the message type definition class M from a given message type definition

libPkg = "ial.base."

importMsg = "import " ++ libPkg ++ "Message;\n"
importTask = "import " ++ libPkg ++ "Task;\n"
importSet = "import java.util.Set;\n"

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


-- Generate a task class from a Task

-- Indents
indent = "    "
indent2 = indent ++ indent
indent3 = indent2 ++ indent
indent4 = indent3 ++ indent
tfClassBegin = "\npublic class "
tfDestVar = indent ++ "private Set<Integer> $"
-- Guard and action methods
tfMsgParam = "(Message _m)"
tfVarMsgSrc = "int $src = _m.getSrc();\n"
-- Guard method
tfGuardMBegin = "public boolean " -- Needs no indent as takes it from source
tfGuardMName = "$Guard"
tfMSep = "\n\n"
-- Action method
tfActionMBegin = "public void " -- Needs no indent as takes it from source
tfActionMName = "$Action"
tfPname = "p"
tfMsgStart = indent2 ++ "{\n" ++ indent3 ++ "Message m_ = new M."
tfMsgSetSrc = indent3 ++ "m_.setSrc($ID);\n"
tfMsgSetDest = indent3 ++ "m_.setDest("
tfMsgEnd = indent3 ++ "send(m_);\n" ++ indent2 ++ "}\n"
tfReplyDestCode = "$src"

-- Generates a Java class file from a Task
-- taskNames: names of the tasks for which destination ID sets should be generated
makeTaskFile :: Task -> [String] -> (String, String)
makeTaskFile t taskNames = ((name t) ++ ".java",
  (prelude t)
  ++ importSet
  ++ importTask
  ++ importMsg
  ++ tfClassBegin ++ (name t) ++ " extends Task {\n"
  ++ concat (map (\name -> tfDestVar ++ name ++ ";\n") taskNames)
  ++ concat (map generateTaskElement (elements t))
  ++ "\n}"
  )

generateTaskElement :: TaskElem -> String
generateTaskElement e =
  case e of
    NormalCharBlock s -> s
    StmntSep -> ";"
    IAP input actionElements -> let varBlock = generateVarBlock (msgT input) (params input) in
      tfGuardMBegin ++ (msgT input) ++ tfGuardMName ++ tfMsgParam ++ " {\n"
      ++ varBlock
      ++ generateGuardExp (cond input) ++ "\n" ++ indent ++ "}"
      ++ tfMSep ++ indent
      ++ tfActionMBegin ++ (msgT input) ++ tfActionMName ++ tfMsgParam ++ " {\n"
      ++ varBlock
      ++ concat (map generateActionElement actionElements) ++ "\n" ++ indent ++ "}"

generateVarBlock msgType params =
  indent2 ++ tfVarMsgSrc
  ++ generateMsgParamVars msgType params

generateMsgParamVars :: String -> [(String, String)] -> String
generateMsgParamVars msgType params = intercalate "\n" (map (generateMsgParamVar msgType) (zip params [1..])) ++ "\n"

generateMsgParamVar :: String -> ((String, String), Integer) -> String
generateMsgParamVar msgType ((t,v),n) =
  indent2 ++ t ++ " " ++ v ++  " = " ++ " ((" ++ "M." ++ msgType ++ ") _m)." ++ tfPname ++ (show n) ++";"


generateGuardExp :: Maybe String -> String
generateGuardExp e = indent ++ indent ++ "return " ++ (fromMaybe "true" e) ++ ";"

generateActionElement :: ActionElem -> String
generateActionElement e =
  case e of
    CodeBlock s -> s
    Sep -> ";"
    Reply {rmsgT=msgType, paramCode=pCode} -> generateSend msgType pCode tfReplyDestCode
    Send {smsgT=msgType, paramCode=pCode, toCode=destCode} -> generateSend msgType pCode destCode

generateSend :: String -> String -> String -> String
generateSend msgType paramCode destCode =
  "\n" ++ tfMsgStart ++ msgType ++ "(" ++ paramCode ++ ";\n"
  ++ tfMsgSetSrc
  ++ tfMsgSetDest ++ destCode ++ ");\n"
  ++ tfMsgEnd

makeParamList :: [(String, String)] -> String
makeParamList ps = "(" ++ intercalate ", " (map (\(t,v) -> t ++ " " ++ v) ps)  ++ ")"

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
