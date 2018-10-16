module Analysis where

import Control.Monad
import Data.Graph
import Data.List
import Data.Maybe

import Parser

-- The representation of IAPs used in the message type graph
data GIAP = GIAP { task :: String, gmsgT :: String, gcond :: Maybe String }
  deriving (Eq, Ord, Show)

-- Short display version of a GIAP
printGIAP :: GIAP -> String
printGIAP x = task x ++ "." ++ gmsgT x ++ condPart
  where condPart = case gcond x of
          Nothing -> ""
          Just c -> "(" ++ c ++ ")"

getCyclicSCCs :: [Task] -> [[GIAP]]
getCyclicSCCs tasks =
  let edges = makeMSGTGraphEdges tasks
      cyclicSCCs = filter (\s -> case s of
                                   CyclicSCC _ -> True
                                   AcyclicSCC _ -> False) (stronglyConnComp edges)
  in map flattenSCC cyclicSCCs

printGIAPList :: [GIAP] -> String
printGIAPList = intercalate ", " . map printGIAP

showCyclicSCCs :: [Task] -> String
showCyclicSCCs = intercalate ", " . map (\s -> "[" ++ s ++ "]") . map printGIAPList . getCyclicSCCs

-- printSCCs :: [Task] -> String
-- printSCCs tasks = -- replace "," ", " .
--   filter (/= '"') . show . map (map printGIAP) $ getSCCs tasks

-- printCyclicSCCs :: [Task] -> String
-- printCyclicSCCs tasks = filter (/= '"') . show . map (map printGIAP) $ getCyclicSCCs tasks

makeMSGTGraph :: [Task] -> (Graph, Vertex -> (GIAP, GIAP, [GIAP]), GIAP -> Maybe Vertex)
makeMSGTGraph tasks = graphFromEdges $ makeMSGTGraphEdges tasks
  -- edges = [(giap, giap, sendsTo) |
  --           (giap, canSendMsgTs) <- getGIAPsFromTasks tasks,
  --           let sendsTo = [giap' | (giap', _) <- getGIAPsFromTasks tasks, (gmsgT giap') `elem` canSendMsgTs] ]

makeMSGTGraphEdges :: [Task] -> [(GIAP, GIAP, [GIAP])]
makeMSGTGraphEdges tasks =
  [(giap, giap, sendsTo) |
    (giap, canSendMsgTs) <- getGIAPsFromTasks tasks,
    let sendsTo = [giap' | (giap', _) <- getGIAPsFromTasks tasks, (gmsgT giap') `elem` canSendMsgTs] ]


-- For a list of Tasks, get a list of the corresponding GIAPs, each together with a list of message types they can send (syntactically, i.e., they include a send or reply statement with that message type)
getGIAPsFromTasks :: [Task] -> [(GIAP, [String])]
getGIAPsFromTasks = join . map getGIAPsFromTask

-- getIAPsFromTasks :: [Task] -> [(String, [String])]
-- getIAPsFromTasks = join . (map (\t -> getIAPsFromTaskElems (elements t)))
-- getIAPsFromTasks = join . (map (\t -> let iaps = getIAPsFromTaskElems (elements t) -- prefix should not be added here
--                                       in map (\(n, acTs) -> (name t ++ "." ++ n, acTs)) iaps))

-- getGIAPsFromTask :: Task -> [GIAP]
-- getGIAPsFromTask t =

getGIAPsFromTask :: Task -> [(GIAP, [String])]
getGIAPsFromTask task = catMaybes $ map (extractIAP $ name task) (elements task)

-- Extracts an IAP in the form (input msg type, [send/reply message types])
extractIAP :: String -> TaskElem -> Maybe (GIAP, [String])
extractIAP name element =
  case element of
    IAP inp ac -> Just (GIAP{task = name, gmsgT = msgT inp, gcond = cond inp}, catMaybes $ map extractMsgT ac)
    _ -> Nothing

-- Extract a msg type if the ActionElem is a reply or send
extractMsgT :: ActionElem -> Maybe String
extractMsgT e =
  case e of
    Reply {rmsgT=t} -> Just t
    Send {smsgT=t} -> Just t
    _ -> Nothing
