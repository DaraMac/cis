{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- make more robust use of Unknown
parseMessage :: String -> LogMessage
parseMessage s =
    case head tkns
    of   "E" -> LogMessage (Error $ read $ tkns !! 1) (read $ tkns !! 2) errorMsg
         "I" -> LogMessage Info    timestamp msg
         "W" -> LogMessage Warning timestamp msg
         _   -> Unknown (unwords tkns)
    where tkns      = words s
          timestamp = read $ tkns !! 1
          msg       = unwords $ drop 2 tkns
          errorMsg  = unwords $ drop 3 tkns


parse :: String -> [LogMessage]
parse = (map parseMessage) . lines 


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node l node r)
    | time msg < time node = Node (insert msg l) node r
    | otherwise            = Node l node (insert msg r)
    where time (LogMessage _ timestamp _) = timestamp


build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l node r) = (inOrder l) ++ [node] ++ (inOrder r)


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map string (filter isError (inOrder $ build msgs))
    where isError (LogMessage (Error n) _ _) = n >= 50
          isError _                          = False
          string (LogMessage _ _ s)          = s
