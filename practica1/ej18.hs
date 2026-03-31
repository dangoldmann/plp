paresDeNat :: [(Int, Int)]
paresDeNat = [(x, s - x) | s <- [0..], x <- [0..s] ]