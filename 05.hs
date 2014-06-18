cands = [9699690, 9699692..]

main :: IO ()
main = putStrLn (show answer)

answer :: Integer
answer = answ 9699690 where
    answ x
        | divis x = x
        | otherwise = answ (x + 2)

divis :: Integer -> Bool
divis x = divisible [2..20] x

divisible :: [Integer] -> Integer -> Bool
divisible ys x = (and . map (\y -> mod x y == 0)) ys
