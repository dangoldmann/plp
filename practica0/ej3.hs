inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso n = Just (1 / n)

aEntero :: Either Int Bool -> Int
aEntero (Right True) = 1
aEntero (Right False) = 0
aEntero (Left n) = n
