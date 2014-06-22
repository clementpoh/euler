{- |
 - Module      :  09.hs
 - Author      :  Clement Poh
 -
 - The following iterative sequence is defined for the set of positive integers:
 -
 - n → n/2 (n is even)
 - n → 3n + 1 (n is odd)
 -
 - Using the rule above and starting with 13, we generate the following sequence:
 -
 - 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
 - It can be seen that this sequence (starting at 13 and finishing at 1)
 - contains 10 terms. Although it has not been proved yet (Collatz Problem), it
 - is thought that all starting numbers finish at 1.
 -
 - Which starting number, under one million, produces the longest chain?
 -
 - NOTE: Once the chain starts the terms are allowed to go above one million.
 -
 - -}
module Main where

import Data.Map as Map hiding (foldl')
import Data.List (foldl')

minit :: Map Integer Integer
minit = fromList [(1, 0), (2, 1)]

answer :: [Integer] -> Integer
answer = fst . foldl' populate (0, minit)

populate :: (Integer, Map Integer Integer) -> Integer -> (Integer, Map Integer Integer)
populate (m, ms) x = case Map.lookup x ms of
    Just n  -> (max m n, ms)
    Nothing -> (max y n, Map.insert x y ns) where
        (n, ns) = populate (m, ms) (next x)
        y = 1 + ns ! next x

next :: Integral a => a -> a
next x = case mod x 2 of
    0 -> div x 2
    _ -> 3 * x + 1
