
{- |
 - Module      :  17.hs
 - Author      :  Clement Poh
 -
 - If the numbers 1 to 5 are written out in words: one, two, three, four, five,
 - then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
 -
 - If all the numbers from 1 to 1000 (one thousand) inclusive were written out
 - in words, how many letters would be used?
 -
 - NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
 - forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
 - letters. The use of "and" when writing out numbers is in compliance with
 - British usage.
 - -}
module Main where

import Data.List (foldl')

answer :: Int
answer = foldl' add 0 [1..1000] where
    add acc n = acc + numChars n

numChars :: Int -> Int
numChars 0 = 0
numChars n 
    | n > 999 = thousands n
    | n > 99 = hundreds n
    | n > 19 = tens n
    | otherwise = length $ ones !! n

thousands :: Int -> Int
thousands x = length "thousand" + numChars y + numChars z where
    y = div x 1000
    z = mod x 1000

hundreds :: Int -> Int
hundreds x = length "hundred" + numChars y + tens' z where
    y = div x 100
    z = mod x 100

tens' :: Int -> Int
tens' 0 = 0
tens' x = length "and" + numChars x

tens :: Int -> Int
tens x = (length . (!!) ts) (div x 10 - 2) + numChars (mod x 10)

ones :: [String]
ones = ["zero", "one", "two", "three", "four", "five", "six", "seven"
        , "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen"
        , "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

ts :: [String]
ts = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
