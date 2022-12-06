module L01A where
import Test.QuickCheck

{-
    Lecture Week 1, part A
    David Sands, 2020
-}

-- Plan: FPFTW

-- Exchange rate calculation
-- Concepts : simple data, types, testing def by cases

-- type infrence vs checking

-- exchangeRate
exchangeRate :: Double
exchangeRate = 10 --12.385 --SEK by Brexit

--functions
fromSEK     :: Double -> Double
fromSEK sek = sek * exchangeRate

fromGBP        :: Double -> Double
fromGBP pounds = pounds / exchangeRate

{--

    1A2

--}
--properties (testings)
prop_exchange     :: Double -> Bool
prop_exchange sek = fromGBP (fromSEK sek) ~== sek

--Random property testing using QuickCheck

m ~== n = abs (m - n) < epsilon 
    where epsilon = 10e-14


{--

    1A3
    
--}

--abs' n = if n < 0 then negate n else n

abs'' n | n < 0 = -n
        | n >= 0 = n     --This is a redudant way to define the function

abs''' n | n < 0 = -n    --Alternative way of definition if the other case will always evaluate to true if the 
       | True = n        -- ... previous one is false

abs' n | n < 0     = -n  --Alternative way of definition if the other case will always evaluate to true if the 
       | otherwise = n   -- ... previous one is false


{--
    1A4
--}

-------------------------------------------------------------------------------------
-- Definition by recursion
-- power n k (computes n ^ k when k is a natural number)
-- power 2 3 = 2 * 2 * 2
-- power 2 2 = 2 * 2

power         :: Integer -> Integer ->Integer
power n k | k == 0      = 1                      --Base case
power n k | k >= 0      = n * power n (k - 1)
power n k | otherwise   = error "Power with a negative exponent not supported!"

--Alternative definition
power'                  :: Integer -> Integer ->Integer
power' n 0               = 1                      --Base case using pattern matching
power' n k | k >= 0      = n * power' n (k - 1)
           | otherwise   = error "Power with a negative exponent not supported!"

--Series of steps of how how power evaluates
-- power 2 3 = 2 * power 2 2 = 2 * 2 * power 2 1 = 2 * 2 * 2 * power 2 0 = 2 * 2 * 2 * 1
example = [ power 2 3
          , 2 * power 2 2
          , 2 * 2 * power 2 1
          , 2 * 2 * 2 * power 2 0
          , 2 * 2 * 2 * 1
          ]

prop_power_wrong n k = n ^ k == power n k -- This definition fails the tests if the power 
                                            --function's values of k is negative

prop_power n k = n ^ (abs k) == power n (abs k) -- Correct definition

prop_power' n k = n ^ k' == power n k'
    where k'  = abs (k) -- Alternative Correct definition
{-
Error Note:: 

in the following definition
power n k | k == 0 = 1                      --Base case
power n k | k > 0 = n * power n (k - 1)


*L01A>power 2 (-2)
*** Exception: L01A.hs:(66,1)-(67,40): Non-exhaustive patterns in function power

--This error means that k being a negative number is not covered by the patterns provided

-}


{-
    1A5 -- Types, type signature, tuples
-}

-- declaring type helps documenting the code and helps understand when there are type error

{-  About Types
    Inspecting types in GHCi,
    browsing what's in scope

-}

-- tuples, lists

--Example 1
--ex1 takes in an integer and returns a tuple of a boolean value True when negative otherwise False,
-- ... and the absolute Integer value

ex1   :: Integer -> (Bool, Integer)
ex1 n =  (n < 0, abs n)

--Definition of fst and snd
fst'        :: (a, b) -> a
fst' (x, _) = x

snd'        :: (a, b) -> b
snd' (_, y) = y

{-
    1A6 Intro to List and Definitions by Pattern Matching
-}
-- tuples, lists
-- More example definitions of tuples

tuple1 :: (Integer, Bool, Double)
tuple1 = (42, True, 9.999)

f1           :: (Integer, Bool, Double) -> (Integer, Double)
f1 (n, b, d)  =  (power n 2, d/2)

string1 :: String
string1 = "Name"

list1 :: [String]
list1 = ["She", "Sells", string1, string1, string1]

summary         :: [String] -> String
summary []       = "Nothing"
summary [s]      = "Just " ++ s
summary [s1, s2] = s1 ++ " and " ++ s2
summary (s:rest) = s ++ " followed by a bunch of things and ending with "
                     ++ last rest

{-
    1A7 Intro to Recursions on Lists
-}
length'        :: [a] -> Integer 
length' []     = 0
length' (x:xs) = 1 + length' xs 

last'       :: [a] -> a
last' []     = error "Empty List!"
last' [x]    = x
last' (_:xs) = last' xs

-- Functions over lists defined by recursions
-- length, last

{-
    1A8 List comprehensions
-}


-- List comprehensions
-- enums 

ten = [1..10]
-- double every element in a list
doubleElems = [2 * n| n <- ten]

-- odd elements of a list
oddElems = [n | n <- ten, odd n]

-- multiple generators
mulGens = [(n, c)|n <- [1, 2, 3], c <- ['a', 'b', 'c']]

mulGensOdd = [(n, c)|n <- [1, 2, 3], c <- ['a', 'b', 'c'], odd n]

mulGensEven = [(n, c)|n <- [1, 2, 3], c <- ['a', 'b', 'c'], even n]

-- QuickSort

qsort []     = []
qsort (n:ns) = qsort smaller ++ [n] ++ qsort larger
    where smaller = [s | s <- ns, s <= n]
          larger  = [b | b <- ns, b >  n]