foldNat :: a -> (a -> a) -> Integer -> a
foldNat casoBase _ 0 = casoBase
foldNat casoBase paso n = paso (foldNat casoBase paso (n - 1))

potencia :: Integer -> Integer -> Integer
potencia base = foldNat 1 (*base) 