{- |
 - Module      :  07.hs
 - Author      :  Clement Poh
 -
 - By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
 - that the 6th prime is 13.
 -
 - What is the 10 001st prime number?
 - -}
module Main where

import Data.Numbers.Primes

answer :: Integer
answer = primes !! 10001
