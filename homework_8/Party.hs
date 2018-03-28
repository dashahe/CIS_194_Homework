{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree


-- exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e g@(GL lst fun) = GL (e : lst) (fun + empFun e) 

instance Monoid GuestList where 
    mempty = GL [] 0
    mappend l1@(GL xs f1) l2@(GL ys f2) = GL (xs ++ ys) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- exercise 2
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f s t@(Node label childs) = f label $ map (treeFold f s) childs

--testTree :: Tree Integer
--testTree = Node 12 [Node 2 [], Node 3 []]

--res :: Integer
--res = treeFold (foldr (+)) 0 testTree

-- exercise 3
combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs boss@(Emp _ fun) emps = moreFun (GL [boss] fun) (foldr1 mappend emps)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss emps = (maximum' withBossList, maximum' withoutBossList)
    where withoutBossList = map fst emps 
          withoutSubBoss  = map snd emps
          withBossList    = map (glCons boss) withoutSubBoss

maximum' :: (Monoid a, Ord a) => [a] -> a
maximum' [] = mempty
maximum' xs = maximum xs

-- exercise 4
maxFun :: Tree Employee -> GuestList
maxFun tree = maxFun (treeFold nextLevel (empty, empty) tree)
    where empty = GL [] 0
          maxFun (a, b) = moreFun a b   

-- exercise 5
main :: IO()
main = readFile "./Company.txt" >>= putStrLn . outputString

outputString :: String -> String
outputString text = gsToString (maxFun tree)
    where tree = read text :: Tree Employee

gsToString :: GuestList -> String
gsToString (GL lst fun) = "Total fun: " ++ show fun ++ "\n" ++ unlines employees
    where employees = map (\(Emp name _) -> name) lst
