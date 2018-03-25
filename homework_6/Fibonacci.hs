{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE FlexibleInstances #-}

-- CIS 194 Homework6 (Lazy equation)

-- exercise 1
-- O(Fn)
fib :: Integer -> Integer

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [fib x | x <- [0..]]

-- exercise 2
-- O(n)
fib' 0 = 0
fib' 1 = 1
fib' n = fibs2 !! (n - 1) + fibs2 !! (n - 2)

fibs2 :: [Integer]
fibs2 = [fib' n | n <- [0..]]

-- exercise 3
-- implement my stream data type and some tool functions for it.
-- It's like a list, but it's infinite of empty always. So it's a good example for lazy evaluation.
data Stream a = Empty | Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList Empty = []
streamToList (Cons a s) = a : streamToList s

-- instance of show for type Stream
instance Show a => Show (Stream a) where 
    --show Empty = []
    --show (Cons a s) = show a ++ show s
    show x = show $ take 20 $ streamToList x

-- x :: Stream Int
-- x = Cons 10 (Cons 9 Empty)

-- exercise 4: some simple tools for Streams
streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap _ Empty     = Empty
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Cons s (streamFromSeed f $ f s)

-- exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap f nums
    where f x = if odd x then 0 else 1 + f (x `div` 2)
          nums = streamFromSeed (+1) 1

-- exercise 6: Fibonacci numbers via generating functions (extra credit)

-- Define an instance of the Num type class for Stream Integer
instance Num (Stream Integer) where
    fromInteger = streamFromSeed (const 0)
    negate = streamMap negate
    (+) (Cons x xs) (Cons y ys) = Cons (x + y) ((+) xs  ys)
    (*) a@(Cons x xs) b@(Cons y ys) = Cons (x * y) (streamMap (*x) ys + xs * b) 

instance Fractional (Stream Integer) where
    (/) (Cons x xs) (Cons y ys) = q
        where q = Cons (x `div` y) (streamMap (`div` y) (xs - q * ys))

-- 0,1,....
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

-- x + x*F(x) + x^2 * F(x) = F(x)
--so....
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)

-- exercise 7： Fibonacci numbers via Maxtrix
-- O(log n) to find the n-th Fibonacci number
data Matrix = Matrix Integer Integer Integer Integer
    deriving Show

instance Num Matrix where
    negate (Matrix a b c d) = Matrix (-a) (-b) (-c) (-d)
    (+) (Matrix a b c d) (Matrix w x y z) = Matrix (a + w) (b + x) ( c + y) (d + z)
    (*) (Matrix a b c d) (Matrix w x y z) = Matrix (a*w+b*y) (a*x+b*z) (c*w+d*y) (c*x+d*z)

getLeftTop :: Matrix -> Integer
getLeftTop m@(Matrix a _ _ _) = a

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = getLeftTop $ fMatrix ^ (n-1)
    where fMatrix = Matrix 1 1 1 0