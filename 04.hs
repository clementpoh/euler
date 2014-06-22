{- |
 - Module      :  04.hs
 - Author      :  Clement Poh
 - 
 - A palindromic number reads the same both ways. The largest palindrome made
 - from the product of two 2-digit numbers is 9009 = 91 × 99.
 -
 - Find the largest palindrome made from the product of two 3-digit numbers.
 - -}
module Main where

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

findPalindrome :: Integer
findPalindrome = maximum $ pal [] 999 999

pal :: [Integer] -> Integer -> Integer -> [Integer]
pal xs 0 0 = xs
pal xs a 0 = pal xs (a - 1) 999
pal xs a b
    | (isPalindrome . show) (a * b) = pal ((a * b) : xs)  a (b - 1)
    | otherwise = pal xs a (b - 1)

