intSquareRoot :: Integer -> Integer
intSquareRoot n = iSqRt 2 n where
    iSqRt a n
        | a * a > n = a - 1
        | otherwise = iSqRt (a + 1) n

factors n = [(x, div n x) | x <- [1..intSquareRoot n], mod n x == 0]

triangle n = sum [1..n]

triangles = map triangle [1..]

answer = head [x | x <- triangles, length (factors x) > 250]
