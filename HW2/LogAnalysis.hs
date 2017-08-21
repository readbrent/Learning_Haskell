{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage (x : xs) =
    case x of
        'I' -> parseInfoMessage (words xs)
        'W'-> parseWarningMessage (words xs)
        'E' -> parseErrorMessage (words xs)
        (_) -> Unknown (x : xs)

parseInfoMessage :: [String] -> LogMessage
parseInfoMessage (timeStamp:message) = 
    LogMessage Info (read timeStamp) (unwords message)

parseWarningMessage :: [String] -> LogMessage
parseWarningMessage (timeStamp : message) =
    LogMessage Warning (read timeStamp) (unwords message)

parseErrorMessage :: [String] -> LogMessage
parseErrorMessage (id : level : message) =
    LogMessage (Error (read id)) (read level) (unwords message)