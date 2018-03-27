module Scrabble where

import Data.Char

-- Hence, the second annotation you decide to implement is one
-- to calowerChare the Scrabble TM score for every line in a buffer. Create a
-- Scrabble module that defines a Score type, a Monoid instance for
-- Score , and the following functions:

newtype Score = Score { getScore :: Int }
    deriving (Show, Eq, Ord)

class Scored a where
    score :: a -> Score 
    
instance Scored a => Scored (a, b) where
    score = score . fst

instance Monoid Score where
    mempty = Score 0
    mappend (Score a) (Score b) = Score (a + b)

instance Scored Char where
    score char
        | lowerChar `elem` "esirantolu" = Score 1 
        | lowerChar `elem` "dg"         = Score 2
        | lowerChar `elem` "cpmb"       = Score 3
        | lowerChar `elem` "hyfvw"      = Score 4
        | lowerChar `elem` "k"          = Score 5
        | lowerChar `elem` "xj"         = Score 8
        | lowerChar `elem` "zq"         = Score 10
        | otherwise                     = Score 0
            where lowerChar = Data.Char.toLower char

scoreString :: String -> Score
scoreString xs = mconcat (score <$> xs)