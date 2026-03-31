mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = foldr (\(x,y) acc -> f x y : acc) []

armarPares :: [a] -> [b] -> [(a, b)]
armarPares [] _ = []
armarPares _ [] = []
armarPares (x:xs) (y:ys) = (x, y) : armarPares xs ys

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f xs ys = mapPares f (armarPares xs ys)