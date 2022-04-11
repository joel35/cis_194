{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage = parseMessage

getMessageType :: String -> MessageType
getMessageType x
    | head x == 'I' = Info
    | head x == 'W' = Warning
    | head x == 'E' = Error 1 -- need to get int from the input string
    | otherwise = getMessageType (tail x)  -- what's a good fallback?



-- Examples
{-
parseMessage "E 2 562 help help"
    == LogMessage (Error 2) 562 "help help"

parseMessage "I 29 la la la"
    == LogMessage Info 29 "la la la"

parseMessage "This is not in the right format"
    == Unknown "This is not in the right format"
-}
