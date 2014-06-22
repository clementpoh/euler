{- |
 - Module      :  09.hs
 - Author      :  Clement Poh
 -
 - A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
 -
 - a2 + b2 = c2
 - For example, 32 + 42 = 9 + 16 = 25 = 52.
 -
 - There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 - Find the product abc.
 - -}
module Main where

answer :: [Integer]
answer = map p $ filter (\(a, b, c) -> a + b + c == 1000) triples where
    p (a, b, c) = a * b * c

triples :: [(Integer, Integer, Integer)]
triples = [(a, b, c)| a <- [3..300], b <- [a..500], c <- [b..500], triple a b c]

triple :: Integer -> Integer -> Integer -> Bool
triple a b c = (a * a) + (b * b) == (c * c)
