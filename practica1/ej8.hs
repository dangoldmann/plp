sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat [] [] = []
sumaMat (f1:xs) (f2:ys) = zipWith (+) f1 f2 : sumaMat xs ys

trasponer :: [[Int]] -> [[Int]]
trasponer = foldr (zipWith (:)) (repeat [])