genLista :: a -> (a -> a) -> Integer -> [a]
genLista _ _ 0 = []
genLista i f n = i : genLista (f i) f (n - 1) 

desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta x1 x2 = genLista x1 (+1) (x2 - x1 + 1)