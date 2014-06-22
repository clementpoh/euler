{- |
 - Module      :  03.hs
 - Author      :  Clement Poh
 - 
 - The prime factors of 13195 are 5, 7, 13 and 29.
 -
 - What is the largest prime factor of the number 600851475143 ?
 - -}
module Main where

import Data.List
import Data.List.Ordered (minus)

goal :: Integer
goal = 600851475143

primes ::[Integer]
primes = (primesTo . intSquareRoot) goal

primesTo :: Integer -> [Integer]
primesTo n = eratos [2..n] where
    eratos [] = []
    eratos (x:xs) = x : eratos (minus xs [x*x, x*x+x..n])

primeFactors :: Integer -> [Integer]
primeFactors n = foldr unpack [] [(x, div n x) | x <- primes, mod n x == 0]
    where unpack (x, y) xs = primeFactors y ++ x : xs

intSquareRoot :: Integer -> Integer
intSquareRoot = intSquareRoot' 2 

intSquareRoot' :: Integer -> Integer -> Integer
intSquareRoot' a n
        | a * a > n = a - 1
        | otherwise = intSquareRoot' (a + 1) n

isPrime :: Integer -> Bool
isPrime n = null $ primeFactors n

answer :: Integer
answer = (maximum . nub . primeFactors) goal

