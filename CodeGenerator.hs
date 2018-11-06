module CodeGenerator where

import Data.List
import Data.Maybe

import Parser

-- Extract all message types from a list of tasks
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
