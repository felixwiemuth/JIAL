module Analysis where

import Data.Maybe

import Parser

getIAPTypes :: [TaskElem] -> [(String, [String])]
getIAPTypes = catMaybes . map extractIAP

-- Extracts an IAP in the form (input msg type, [send/reply message types])
extractIAP :: TaskElem -> Maybe (String, [String])
extractIAP e =
  case e of
    IAP inp ac -> Just (msgT inp, catMaybes $ map extractMsgT ac)
    _ -> Nothing

-- Extract a msg type if the ActionElem is a reply or send
extractMsgT :: ActionElem -> Maybe String
extractMsgT e =
  case e of
    Reply {rmsgT=t} -> Just t
    Send {smsgT=t} -> Just t
    _ -> Nothing
