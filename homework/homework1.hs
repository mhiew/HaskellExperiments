module Homework1
( toDigits
, toDigitsRev
, doubleEveryOther
, sumDigits
, validate
, hanoi
, hanoi4
) where 

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


-- Exercise 5

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi numDisks source destination storage 
 | numDisks <= 0 = []
 | otherwise = hanoi (numDisks - 1) source storage destination      -- move all but the last disk to storage
                ++ [(source, destination)]                          -- move  the last (largest) disk to destination
                ++ hanoi (numDisks -1) storage destination source   -- move all storage disks to desintation


-- Exercise 6

-- helper function to calculate the top half 
topHalf :: Integer -> Integer
topHalf x = x `div` 2   -- relies truncating decimals

-- helper function to calculate bottom half excluding last disk
bottomHalf :: Integer -> Integer
bottomHalf x = ((x - 1) `div` 2) -- relies on truncating decimals

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move] -- function overloading didn't work. maybe they teach it to use later...for now suffix with 4
hanoi4 numDisks source destination storage1 storage2
 | numDisks <= 0 = []
 | otherwise = hanoi4 (topHalf numDisks) source storage1 destination storage2       -- move topHalf to storage 1 can use both storage as empty
                ++ hanoi (bottomHalf numDisks) source storage2 destination          -- move bottomHalf excluding last to storage 2 (storage1 is off limits)
                ++ [(source, destination)]                                          -- move last disk (biggest disk) to destination 
                ++ hanoi (bottomHalf numDisks) storage2 destination source          -- move bottomHalf excluding last to destination (source is valid (empty), storage1 is off limits)
                ++ hanoi4 (topHalf numDisks) storage1 destination source storage2   -- move topHalf to destination can use all pegs as all platforms hold bigger disks


-- convenience test method with predefined pegs
hanoi4Test :: Integer -> [Move]
hanoi4Test numDisks = hanoi4 numDisks "a" "b" "c" "d"
