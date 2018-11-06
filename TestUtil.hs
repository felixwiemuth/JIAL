module TestUtil where

import Parser

-----------------------------------------
-- Helper functions to generate test IAPs
-----------------------------------------

-- Generate an IAP with the given input message type, sending the given message types
iap :: String -> [String] -> TaskElem
iap inputType sendTypes = IAP (Input {msgT = inputType, params = [], cond = Nothing})
                                (map (\sendType -> Send {sp1 = "", smsgT = sendType, sp2 = "", paramCode = "", toCode = ""}) sendTypes)
