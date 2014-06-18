module Main where

fibs :: [Integer]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

fibLimit :: [Integer]
fibLimit = takeWhile (< 4000000) fibs

fibEven :: [Integer]
fibEven = [x | x <- fibLimit, mod x 2 == 0]
