{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

-- CIS 194 homework 7

module JoinList where
import Data.Monoid
import Buffer
import Editor
import Scrabble
import Sized

-- exercise 1
{-
data JoinListBasic a = Empty
                     | Single a
                     | Append (JoinListBasic a) (JoinListBasic a)


jlbToList :: JoinListBasic a -> [a]
jlbToList Empty = []
jlbToList (Single a) = [a]
jlbToList (Append l1 l2) = jlbToList l1 ++ jlbToList l2
-}

-- The m parameter will be used to track monoidal annotations to the structure. 
-- The idea is that the annotation at the root of a JoinList
-- will always be equal to the combination of all the annotations on the Single nodes.

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
        deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l1 l2 = Append (tag l1 `mappend` tag l2) l1 l2

tag :: Monoid m => JoinList m a -> m 
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- for test:
-- testTree :: JoinList (Product Int) a
-- testTree = Append (Product 2) Empty Empty

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n (Single _ x)
    | n == 0     = Just x
    | otherwise  = Nothing
indexJ n (Append m l1 l2)
    | n < 0 || n > thisSize = Nothing
    | n < leftSize          = indexJ n l1
    | otherwise             = indexJ (n - leftSize) l2
        where thisSize = getSize . size $ m  
              leftSize = getSize . size . tag $ l1


jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

-- The dropJ function drops the first n elements from a JoinList .
-- This is analogous to the standard drop function on lists. 
-- shoule be: jlToList (dropJ n jl) == drop n (jlToList jl).
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n a@(Single _ x)
    | n >= 1    = Empty
    | otherwise = a
dropJ n a@(Append m l1 l2)
    | n >= thisSize  = Empty
    | n >= leftSize  = dropJ (n - leftSize) l2
    | n > 0          = dropJ n l1 +++ l2
    | otherwise      = a
        where thisSize = getSize . size $ m
              leftSize = getSize . size . tag $ l1 
-- should be: (indexJ i jl) == (jlToList jl !!? i)
-- for test:
-- testTree :: JoinList Size String
-- testTree = Append (Size 2) (Single (Size 1) "Hello world") (Single (Size 1) "a")

{-
            *JoinList> jlToList $  dropJ (-1) testTree
            ["Hello world","a"]
            *JoinList> jlToList $  dropJ (0) testTree
            ["Hello world","a"]
            *JoinList> jlToList $  dropJ (1) testTree
            ["a"]
            *JoinList> jlToList $  dropJ (2) testTree
            []
            *JoinList> jlToList $  dropJ (3) testTree
            []
-}

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n Empty = Empty
takeJ n a@(Single _ _)
    | n >= 1    = a
    | otherwise = Empty
takeJ n a@(Append m l1 l2)
    | n >= thisSize = a
    | n >= leftSize = l1 +++ takeJ (n - leftSize) l2
    | n > 0         = takeJ n l1
    | otherwise     = Empty
        where thisSize = getSize . size $ m
              leftSize = getSize . size . tag $ l1


-- exercise 3
scoreLine :: String -> JoinList Score String
scoreLine str = case words str of 
    []      -> Empty
    [x]     -> Single (scoreString x) x
    x:xs    -> Append (tag l <> tag r) l r
                    where l = scoreLine x
                          r = scoreLine . unwords $ xs

test :: JoinList Score String
test = scoreLine "what are you doing man  I don't you about you"

-- exercise 4
--instance (Monoid a, Monoid b) => Monoid (a,b) where
--    mempty = (mempty, mempty)
--    mappend (a1,b1) (a2,b2) = (mappend a1 a2, mappend b1 b2)

-- make an instance of Buffer for JoinList (Score, Size) String
instance Buffer (JoinList (Score, Size) String) where
    toString = unlines . jlToList
    
    fromString xs = foldr1 (+++) $ createLines <$> lines xs
        where createLines s = Single (scoreString s, Size 1) s
    
    line = indexJ

    replaceLine _ _ Empty = Empty
    replaceLine n str a@(Single _ s)
        | n == 0    = fromString str 
        | otherwise = a
    replaceLine n str a@(Append m l r)
        | n >= thisSize || n < 0 = a
        | n >= leftSize         = l +++ replaceLine (n - leftSize) str r
        | n >= 0                = replaceLine n str l +++ r
        where thisSize = getSize . size $ m
              leftSize = getSize . size . tag $ l

    numLines = getSize . size . tag
    value    = getScore . fst . tag

testBuffer :: JoinList (Score, Size) String
testBuffer = fromString . unlines $ [
    "• v — view the current location in the document",
    "• n — move to the next line",
    "• p — move to the previous line",
    "• l — load a file into the editor",
    "• e — edit the current line",
    "• q — quit",
    "• ? — show this list of command"
    ]

main = runEditor editor testBuffer