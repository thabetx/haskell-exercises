module SumOfMultiples (sumOfMultiples) where
import Data.List

muls :: [Integer] -> Integer -> [Integer]
muls [] limit = []
muls (x:xs) limit = [ y | y <- [1..limit-1], y `mod` x == 0] ++ muls xs limit

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ nub $ muls factors limit


