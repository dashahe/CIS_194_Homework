-- problem one: Validating Credit Card Numbers 1 --

doubleFirstNum :: [Integer] -> [Integer]
doubleLastSecond :: [Integer] -> [Integer]
numToDigits :: Integer -> [Integer]
sumDigits :: [Integer] -> Integer
isValid :: Integer -> Bool

cardID :: Integer

doubleFirstNum nums = head nums * 2 : drop 1 nums

doubleLastSecond nums = take (length nums - 2) nums 
    ++ doubleFirstNum (drop (length nums - 2) nums)

numToDigits 0 = []
numToDigits x = numToDigits (x `div` 10) ++ [x `mod` 10]

sumDigits nums = sum [sum (numToDigits x) | x <- nums]

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

main = do
    print (isValid cardID)