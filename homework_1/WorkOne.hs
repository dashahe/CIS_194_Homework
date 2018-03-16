-- homework 1 for cis 194 --
-- problem one --

doubleFirstNum :: [Integer] -> [Integer]
doubleLastSecond :: [Integer] -> [Integer]
numToDigits :: Integer -> [Integer]
sumDigits :: [Integer] -> Integer
isValid :: Integer -> Bool

cardID :: Integer

doubleFirstNum numLst = head numLst * 2 : drop 1 numLst

doubleLastSecond numLst = take (length numLst - 2) numLst 
    ++ doubleFirstNum (drop (length numLst - 2) numLst)

numToDigits 0 = []
numToDigits x = numToDigits (x `div` 10) ++ [x `mod` 10]

sumDigits numLst = sum [sum (numToDigits x) | x <- numLst]

cardID = 112121212121212
isValid cardID = tail (
    numToDigits (
        sumDigits (
            doubleFirstNum (
                numToDigits (
                    cardID)
                )
            )   
        )
    ) == [8]