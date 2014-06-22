{- |
 - Module      :  09.hs
 - Author      :  Clement Poh
 -
 - 215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 -
 - What is the sum of the digits of the number 21000?
 - -}
module Main where

import Data.List (foldl')
import Data.Char

digitSum :: Integer -> Integer -> Int
digitSum b e = foldl' add 0 $ show (b ^ e) where
    add acc x = acc + digitToInt x
