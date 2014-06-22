{- |
 - Module      :  15.hs
 - Author      :  Clement Poh
 -
 - Starting in the top left corner of a 2×2 grid, and only being able to move
 - to the right and down, there are exactly 6 routes to the bottom right
 - corner.
 -
 - How many such routes are there through a 20×20 grid?
 - -}
module Main where

answer :: Integer
answer = paths 20 20

paths :: Integer -> Integer -> Integer
paths x y = choose (x + y - 1) y

choose :: Integer -> Integer -> Integer
choose _ 0 = 1
choose 0 _ = 1
choose n r = choose (n - 1) (r - 1) * div n r
