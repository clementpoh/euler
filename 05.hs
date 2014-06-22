{- |
 - Module      :  05.hs
 - Author      :  Clement Poh
 -
 - 2520 is the smallest number that can be divided by each of the numbers from
 - 1 to 10 without any remainder.
 -
 - What is the smallest positive number that is evenly divisible by all of the
 - numbers from 1 to 20?
 - -}
module Main where

cands :: [Integer]
cands = [9699690, 9699692..]

main :: IO ()
main = print (show answer)

answer :: Integer
answer = answ 9699690 where
    answ x
        | divis x = x
        | otherwise = answ (x + 2)

divis :: Integer -> Bool
divis = divisible [2..20]

divisible :: [Integer] -> Integer -> Bool
divisible ys x = all (\y -> mod x y == 0) ys
