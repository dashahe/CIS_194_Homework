-- CIS 194 Homework5
{-# LANGUAGE FlexibleInstances #-}

module Calc where 

import ExprT
import Parser
import Data.Maybe
import qualified Data.Map as M

-- exercise 1: an evaluator for ExprT
eval :: ExprT -> Integer
eval expr = case expr of 
    (Lit x)     -> x
    (Add l r)   -> eval l + eval r
    (Mul l r)   -> eval l * eval r

-- exercise 2: 
evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of 
    Nothing     -> Nothing
    Just x      -> Just (eval x)


-- exercise 3: implement type class Expr
-- constructor class
class Expr a where 
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id


-- exercise 4: 
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
    lit x = Mod7 $ x `mod` 7
    add (Mod7 x) (Mod7 y) = Mod7((x + y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7((x * y) `mod` 7)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(2*-4)+5+10"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

-- exercise 6
class HasVars a where
    var :: String -> a

data VarExprT = VarExprT String Integer
        deriving (Show, Eq)

instance HasVars VarExprT where
    var str = VarExprT str 0

instance Expr VarExprT where 
    lit = VarExprT ""
    add (VarExprT _ a) (VarExprT _ b) = VarExprT "" (a + b)
    mul (VarExprT _ a) (VarExprT _ b) = VarExprT "" (a * b)

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit a _ = Just a
    add f g m = if isNothing  (f m) || isNothing (g m)
        then Nothing
        else Just (fromJust (f m) + fromJust (g m))
    mul f g m = if isNothing (f m) || isNothing (g m)
        then Nothing
        else Just (fromJust (f m) * fromJust (g m))

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs