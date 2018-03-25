{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

-- CIS 194 homework 7

module JoinList where
import Data.Monoid
import Buffer
import Editor
import Scrabble
import Sized

-- exercise 1
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

