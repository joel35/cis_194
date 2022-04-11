-- Learn You A Haskell - Starting Out

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else doubleMe x

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]


-- CIS 194: Homework 1
--
-- Validating Credit Card Numbers
--
-- In this section, you will implement the validation algorithm for
-- credit cards. It follows these steps:
--
-- • Double the value of every second digit beginning from the right.
--   That is, the last digit is unchanged; the second-to-last digit is doubled; the third-to-last digit is unchanged; and so on. For example,
--   [1,3,8,6] becomes [2,3,16,6].
--
-- • Add the digits of the doubled values and the undoubled digits from the original number. For example, [2,3,16,6] becomes
--   2+3+1+6+6 = 18.
--
-- • Calculate the remainder when the sum is divided by 10. For the
--   above example, the remainder would be 8.
--
-- If the result equals 0, then the number is valid.
--
-- Exercise 1
-- We need to first find the digits of a number. Define the
-- functions
-- 
-- toDigits :: Integer -> [Integer]
-- toDigitsRev :: Integer -> [Integer]
-- 
-- toDigits should convert positive Integers to a list of digits. (For 0 or
-- negative inputs, toDigits should return the empty list.) toDigitsRev
-- should do the same, but with the digits reversed.

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise =  toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

checkEx1 :: [Bool]
checkEx1 = [
    toDigits 1234 == [1,2,3,4],
    toDigitsRev 1234 == [4,3,2,1],
    null (toDigits 0),
    null (toDigits (-17))
    ]

-- Exercise 2
-- Once we have the digits in the proper order, we need to
-- double every other one. Define a function

-- doubleEveryOther :: [Integer] -> [Integer]

-- Remember that doubleEveryOther should double every other number beginning from the right, that is, the second-to-last, fourth-to-last,
-- . . . numbers are doubled.

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleOddElements (reverse xs))

doubleOddElements :: [Integer] -> [Integer]
doubleOddElements [] = []
doubleOddElements [x] = [x]
doubleOddElements (x:y:zs) = x:y*2:doubleOddElements zs

checkEx2 :: [Bool]
checkEx2 = [
    doubleEveryOther [8,7,6,5] == [16,7,12,5],
    doubleEveryOther [1,2,3] == [1,4,3]
    ]


-- Exercise 3
-- The output of doubleEveryOther has a mix of one-digit
-- and two-digit numbers. Define the function
-- sumDigits :: [Integer] -> Integer
-- to calculate the sum of all digits.
-- Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (expandList xs)

expandList :: [Integer] -> [Integer]
expandList [] = []
expandList [x] = [x]
expandList (x:xs) = toDigits x ++ expandList xs

checkEx3 :: [Bool]
checkEx3 = [sumDigits [16,7,12,5] == 22]

-- Exercise 4
-- Define the function
-- validate :: Integer -> Bool
-- that indicates whether an Integer could be a valid credit card number. This will use all functions defined in the previous exercises.
-- Example: validate 4012888888881881 = True
-- Example: validate 4012888888881882 = False

validate :: Integer -> Bool 
validate x =  modTen (sumDigits (doubleEveryOther (toDigits x))) == 0

modTen :: Integer -> Integer
modTen x = x `mod` 10

checkEx4 :: [Bool]
checkEx4 = [
    validate 4012888888881881,
    not (validate 4012888888881882)
    ]


-- Exercise 5
-- The Towers of Hanoi is a classic puzzle with a solution
-- that can be described recursively. Disks of different sizes are stacked
-- on three pegs; the goal is to get from a starting configuration with
-- all disks stacked on the first peg to an ending configuration with all
-- disks stacked on the last peg, as shown in Figure 1.
-- 
-- The only rules are
--  • you may only move one disk at a time, and
--  • a larger disk may never be stacked on top of a smaller one.
-- 
-- For example, as the first move all you can do is move the topmost,
-- smallest disk onto a different peg, since only one disk may be moved
-- at a time.
-- 
-- To move n discs (stacked in increasing size) from peg a to peg b
-- using peg c as temporary storage,
--  1. move n − 1 discs from a to c using b as temporary storage
--  2. move the top disc from a to b
--  3. move n − 1 discs from c to b using a as temporary storage.
-- 
-- For this exercise, define a function hanoi with the following type:
-- 
-- type Peg = String
-- type Move = (Peg, Peg)
-- hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- 
-- Given the number of discs and names for the three pegs, hanoi
-- should return a list of moves to be performed to move the stack of
-- discs from the first peg to the second.
-- 
-- Note that a type declaration, like type Peg = String above, makes
-- a type synonym. In this case Peg is declared as a synonym for String,
-- and the two names Peg and String can now be used interchangeably.
-- Giving more descriptive names to types in this way can be used to
-- give shorter names to complicated types, or (as here) simply to help
-- with documentation.
-- 
-- Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]

type Peg = String 
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = 
    let
        step1moves = hanoi (n-1) a c b
        step2move  = (a, b)
        step3moves = hanoi (n-1) c b a
    in
        step1moves ++ [step2move] ++ step3moves