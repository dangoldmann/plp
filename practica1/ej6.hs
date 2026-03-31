recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

-- sacarUna :: Eq a => a -> [a] -> [a]
-- sacarUna _ [] = []
-- sacarUna e (x:xs) = if e == x then xs else x : sacarUna e xs

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs acc -> if x == e then xs else x : acc) []

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs acc -> if e <= x then e : x : xs else x : acc) [e]