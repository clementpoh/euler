import Data.Numbers.Primes

answer :: Integer
answer = (sum . takeWhile (< 2000000)) primes
