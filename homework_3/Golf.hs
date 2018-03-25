-- CIS 194 Homework 3
-- my task is to make the code as short as possible
module Golf where

-- The first list in the output should be the same as the input list. 
-- The second list in the output should contain every second element from the input list. . . 
-- and the nth list in the output should contain every nth element from the input list.

-- findNthList is actuallt a function which products a list contains every n-step element.
-- drop the n-1 elements in the begin, and then get the list contains all n-step elements.
skips :: [a] -> [[a]]
skips xs = [findNthList n (drop (n - 1) xs) | n <- [1..length xs]]
    where findNthList n [] = [] 
          findNthList n (x:xs) = x : findNthList n (drop (n - 1) xs)


-- A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it. For example, 
-- in the list [2,3,4,1,5], the only local maximum is 4, since it is greater than 
-- the elements immediately before and after it (3 and 1). 
-- 5 is not a local maximum since there is no element that comes after it.

-- First, I use triples to generate all possible triples. 
-- And then, I filte all triples which the mid is larger than the first and last.
localMaxima :: [Integer] -> [Integer]
localMaxima xs = [x !! 1 | x <- triples xs, head x < x !! 1, last x < x !! 1]
    where triples (x:y:z:zs) = [x, y, z] : triples (y:z:zs)  
          triples _ = []


-- For this task, write a function histogram :: [Integer] -> String
-- which takes as input a list of Integers between 0 and 9 (inclusive),
-- and outputs a vertical histogram showing how many of each number
-- were in the input list. You may assume that the input list does not
-- contain any numbers less than zero or greater than 9 (that is, it does
-- not matter what your function does if the input does contain such
-- numbers). 

histogram :: [Integer] -> String
histogram xs = fun (countTimes xs) ++ "\n==========\n" ++ "0123456789"

fun :: [Int] -> String
fun xs 
    | maximum xs <= 0 = ""
    | otherwise = fun (map (+ negate 1) xs) ++ "\n" ++ [if x > 0 then '*' else ' ' | x <- xs]

countTimes :: [Integer] -> [Int]
countTimes xs = [length (filter (==n) xs) | n <- [0..9]]

{- test result:
*Golf> putStrLn (histogram [0,2,2,2,2,2,1,1,1,3,4,5,3,2,4])

  *       
  *       
  *       
 **       
 ****     
******    
==========
0123456789

-}