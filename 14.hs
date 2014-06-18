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
