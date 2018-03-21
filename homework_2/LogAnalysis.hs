-- CIS 194 Homework 2

module LogAnalysis  (
    parse
   ,parseMessage
   ,insert
   ,build
   ,inOrder
) where

import Log

parse :: String -> [LogMessage]
parse = map parseMessage . lines

parseMessage :: String -> LogMessage
parseMessage logStr = case words logStr of
    ("I":t:s)   -> LogMessage  Info    (read t :: Int)  (unwords s)
    ("W":t:s)   -> LogMessage  Warning (read t :: Int)  (unwords s)
    ("E":i:t:s) -> LogMessage  (Error  (read i :: Int)) (read t :: Int) (unwords s)
    _           -> Unknown logStr


insert :: LogMessage -> MessageTree -> MessageTree
-- LogMessage{} equals to (LogMessage _ _ _)
-- if insert a Unknown LogMessage
insert logMsg@(Unknown _) tree = tree
-- if insert a LogMessage to Leaf
insert logMsg@LogMessage{} Leaf = Node Leaf logMsg Leaf
-- other cases
insert logMsg1@(LogMessage _ ts1 _) (Node left logMsg2@(LogMessage _ ts2 _) right)
    | ts1 > ts2 = Node left logMsg2 (insert logMsg1 right)
    | otherwise = Node (insert logMsg1 left) logMsg2 right


build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMsg right) = inOrder left ++ [logMsg] ++ inOrder right

-- use pattern-matching to filter all non-error log messages
isRelevant :: LogMessage -> Bool
isRelevant (LogMessage (Error t) _ _)
    | t > 50 = True
    | otherwise = False
isRelevant _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = foldr (\(LogMessage (Error _) _ s) acc -> s : acc) [] xs
    where xs = inOrder $ build $ filter isRelevant logs

{----my test----
*LogAnalysis> str = "I 6 Completed armadillo processing\n\nI 1 Nothing to report\n\nI 4 Everything normal\n\nI 11 Initiating self-destruct sequence\n\nE 70 3 Way too many pickles\n\nE 65 8 Bad pickle-flange interaction detected\n\nW 5 Flange is due for a check-up\n\nI 7 Out for lunch, back in two time steps\n\nE 20 2 Too many pickles\n\nI 9 Back from lunch\n\nE 99 10 Flange failed!"
*LogAnalysis> whatWentWrong . parse $ str
["Way too many pickles","Bad pickle-flange interaction detected","Flange failed!"]
-}

{-
*LogAnalysis> testWhatWentWrong parse whatWentWrong "error.log"
["Mustardwatch opened, please close for proper functioning!","All backup mustardwatches are busy","Depletion of mustard stores detected!","Hard drive failure: insufficient mustard","Twenty seconds remaining until out-of-mustard condition","Ten seconds remaining until out-of-mustard condition","Empty mustard reservoir! Attempting to recover...","Recovery failed! Initiating shutdown sequence"]
-}