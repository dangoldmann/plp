valorAbsoluto :: Float -> Float
valorAbsoluto n | n >= 0 = n
                | otherwise = -n

bisiesto :: Int -> Bool
bisiesto a | mod a 400 == 0 = True
           | mod a 100 == 0 = False
           | otherwise = mod a 4 == 0

factorial :: Int -> Int
factorial 0 = 1
factorial n = factorial (n - 1) * n

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = sumarCantidadDivisoresPrimos n 1

sumarCantidadDivisoresPrimos :: Int -> Int -> Int
sumarCantidadDivisoresPrimos a b | a == b && esPrimo a = 1
sumarCantidadDivisoresPrimos a b | a == b = 0
sumarCantidadDivisoresPrimos a b | mod a b == 0 && esPrimo b = 1 + sumarCantidadDivisoresPrimos a (b + 1)
                                 | otherwise = sumarCantidadDivisoresPrimos a (b + 1)

esPrimo :: Int -> Bool
esPrimo n = n > 1 && menorDivisor n == n

menorDivisor :: Int -> Int
menorDivisor n = buscarDivisor n 2

buscarDivisor :: Int -> Int -> Int
buscarDivisor a b | mod a b == 0 = b
                  | otherwise = buscarDivisor a (b + 1)