{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
--
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
--
-- parseMessage "This is not in the right format" == Unknown "This is not in the right format"

parseMessage :: String -> LogMessage
parseMessage s =
    let parts = words s
     in case parts of
            ("I" : ts : msg) -> LogMessage Info (read ts) (unwords msg)
            ("W" : ts : msg) -> LogMessage Warning (read ts) (unwords msg)
            ("E" : lvl : ts : msg) -> LogMessage (Error (read lvl)) (read ts) (unwords msg)
            _ -> Unknown (unwords parts)

-- Version ohne `map`
parse :: String -> [LogMessage]
parse file =
    let l = lines file
     in parseLines l
  where
    parseLines :: [String] -> [LogMessage]
    parseLines [] = []
    parseLines (s : ss) = parseMessage s : parseLines ss

-- Version mit `map`
parse' :: String -> [LogMessage]
parse' = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tr = tr
insert message@LogMessage{} Leaf = Node Leaf message Leaf -- Synonym zu (LogMessage _ _ _)
insert message@(LogMessage _ tMes _) (Node leftSub originalMessage@(LogMessage _ tNode _) rightSub)
    | tMes < tNode = Node (insert message leftSub) originalMessage rightSub
    | tMes > tNode = Node leftSub originalMessage (insert message rightSub)
    | otherwise = error "insert: Time conflict. Two messages with identical time stamp."
insert _ _ = error "insert: Unknown message types or tree kaputt."

build :: [LogMessage] -> MessageTree
-- build [] = Leaf
-- build (m : ms) = insert m (build ms)
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
    Leaf -> []
    (Node leftSub msg rightSub) -> inOrder leftSub ++ [msg] ++ inOrder rightSub

-- Nimmt unsortierte Liste von LogMessage an und gibt sortiert nach Zeit diejenigen zurück, die Fehler mit Schwere 50 oder größer sind.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
    readMsg . inOrder . build . isSevereError
  where
    isSevereError =
        filter
            --         (\msg -> case msg of (LogMessage (Error s) _ _) -> s > 50; _ -> False)

            (\case (LogMessage (Error s) _ _) -> s > 50; _ -> False)
    readMsg ((LogMessage _ _ m) : ms) = m : readMsg ms
    readMsg _ = []
