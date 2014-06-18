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

