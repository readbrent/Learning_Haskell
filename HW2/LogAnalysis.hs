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

parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)

getTimestamp :: LogMessage -> TimeStamp
getTimestamp (Unknown _) = error "Invalid input"
getTimestamp (LogMessage Info      timestamp _) = timestamp
getTimestamp (LogMessage Warning   timestamp _) = timestamp
getTimestamp (LogMessage (Error _) timestamp _) = timestamp

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert newMessage (Node left currentMessage right)
    | getTimestamp newMessage <= getTimestamp currentMessage = Node (insert newMessage left) currentMessage right
    | otherwise                                              = Node left currentMessage (insert newMessage right)

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left thisMessage right) = (inOrder left) ++ [thisMessage] ++ (inOrder right) 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logList = map getMessage (filter isSevereLog logList)
    where 
        isSevereLog ::LogMessage -> Bool
        isSevereLog (LogMessage (Error n) _ _)
            | n >= 50 = True
            | otherwise = False
        isSevereLog _ = False

        getMessage :: LogMessage -> String
        getMessage (Unknown message) = message
        getMessage (LogMessage Info      _ message) = message
        getMessage (LogMessage Warning   _ message) = message
        getMessage (LogMessage (Error _) _ message) = message