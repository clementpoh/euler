{- |
 - Module      :  10.hs
 - Author      :  Clement Poh
 -
 - The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 -
 - Find the sum of all the primes below two million.
 - -}
module Main where

import Data.Numbers.Primes

answer :: Integer
answer = (sum . takeWhile (< 2000000)) primes
