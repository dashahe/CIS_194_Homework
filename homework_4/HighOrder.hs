-- CIS 194 Homework 4

-- exercise 1: wholemeal programming

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- fun1 is a function actually product all even nums in num list.
fun1' :: [Integer] -> Integer
fun1' = product . filter even . map (subtract 2)

-- fun2 is a function actually sum all even nums in the num list.
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1) 



-- exercise 2: folding with trees
data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show, Eq)

depth :: Tree a -> Int
depth tree = case tree of 
    Leaf                     -> 0
    tree@(Node left a right) -> depth left + depth right + 1  

insert :: a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x tree@(Node left a right)
    | depth left < depth right = Node (insert x left) a right
    | otherwise                = Node left a (insert x right)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- test whether the tree is balanced or not
isBalance :: Tree a -> Bool
isBalance Leaf = True
isBalance tree@(Node left _ right) = 
    abs (depth left - depth right) < 2 && isBalance left && isBalance right  


-- exercise 3: more folds
xor :: [Bool] -> Bool
xor = foldr (\x acc -> if x then not(x && acc) else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- implement foldl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base $ reverse xs  

-- exercise 4: finding primes
intSqrt :: Integer -> Integer
intSqrt = floor .sqrt . fromIntegral

cardProd :: [a] -> [b] -> [(a, b)]
cardProd xs ys = [(x, y) | x <- xs, y <- ys]

findOddComposite :: Integer -> [Integer]
findOddComposite n =  takeWhile (<2*n+2) . map cal . filter (uncurry(>=)) $ cardProd m m
    where cal (i, j) = 2 * (i + j + 2 * i * j) + 1
          m = [1..intSqrt n + 1]

-- use Sundaram sieve method to find primes
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = filter odd $ filter (`notElem` coms) [2..2*n+2]
    where coms = findOddComposite n