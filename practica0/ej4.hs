limpiar :: String -> String -> String
limpiar [] s = s
limpiar (x:xs) s = limpiar xs (limpiarCaracter x s)

limpiarCaracter :: Char -> String -> String
limpiarCaracter _ [] = []
limpiarCaracter c (x:xs) | c == x = limpiarCaracter c xs
                         | otherwise = x : limpiarCaracter c xs

difPromedio :: [Float] -> [Float]
difPromedio l = calcularDifPromedios l (calcularPromedioGeneral l 0 (length l))

calcularDifPromedios :: [Float] -> Float -> [Float]
calcularDifPromedios [] _ = []
calcularDifPromedios (x:xs) p = x - p : calcularDifPromedios xs p

calcularPromedioGeneral :: [Float] -> Float -> Int -> Float
calcularPromedioGeneral [] suma tamaño = suma / fromIntegral tamaño
calcularPromedioGeneral (x:xs) suma tamaño = calcularPromedioGeneral xs (suma + x) tamaño

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales (x:xs) = todosIgualesAux xs x

todosIgualesAux :: [Int] -> Int -> Bool
todosIgualesAux [] _ = True
todosIgualesAux (x:xs) e | x /= e = False
                         | otherwise = todosIgualesAux xs e