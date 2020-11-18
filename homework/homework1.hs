-- Exercise 1

-- helper function to recursively reverse a list
-- [1,2,3,4] = [4,3,2,1]
recursiveReverse :: [Integer] -> [Integer]
recursiveReverse [] = []
recursiveReverse (x:[]) = [x]
recursiveReverse (x:ys) = recursiveReverse ys ++ [x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
 | n <= 0 = []
 | n < 10 = [n]
 | otherwise = n `rem` 10 : toDigitsRev (n `div` 10) --evidently (n / 10) doesn't work for integers hence `div`
 
toDigits :: Integer -> [Integer]
toDigits n = recursiveReverse (toDigitsRev n)


-- Exercise 2

-- helper function that doubles every other from left to right
-- [1,2,3,4] = [1,4,3,8]
doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft (x:[]) = [x]
doubleEveryOtherFromLeft (x:y:zs) = [x, y*2] ++ doubleEveryOtherFromLeft zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = recursiveReverse (doubleEveryOtherFromLeft (recursiveReverse n))


-- Exercise 3

-- helper function that converts an array of integers into array of significant digits
-- [111, 23, 5] = [1,1,1,2,3,5]
spreadDigits :: [Integer] -> [Integer]
spreadDigits [] = []
spreadDigits (x:ys) = toDigits x ++ spreadDigits ys

sumDigits :: [Integer] -> Integer
sumDigits n = sum (spreadDigits n)


-- Exercise 4

-- checksum algorithm for validating credit cards:
-- 1. Double the value of every second digit beginning from the right.
-- 2. Add the digits of the doubled values and the undoubled digits from the original number.
-- 3. Calculate the remainder when the sum is divided by 10. If the result is 0 the number is valid.
validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `rem` 10 == 0
