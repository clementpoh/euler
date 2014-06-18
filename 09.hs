answer = map p $ filter (\(a, b, c) -> a + b + c == 1000) triples where
    p (a, b, c) = a * b * c

triples :: [(Integer, Integer, Integer)]
triples = [(a, b, c)| a <- [3..300], b <- [a..500], c <- [b..500], triple a b c]

triple :: Integer -> Integer -> Integer -> Bool
triple a b c = (a * a) + (b * b) == (c * c)
