permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs = concatMap elegir [0..length xs - 1]
    where
        elegir i =
            let x = xs !! i
                resto = take i xs ++ drop (i + 1) xs
            in map (x:) (permutaciones resto)

-- partes :: [a] -> [[a]]
-- partes [] = [[]]
-- partes (x:xs) = ps ++ map (x:) ps
--     where
--         ps = partes xs

partes :: [a] -> [[a]]
partes = foldr (\x acc -> acc ++ map (x:) acc) [[]]

prefijos :: [a] -> [[a]]
prefijos xs = map (\i -> take i xs) [0..length xs]


sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas xs = [] : concatMap elegir [0..length xs - 1]
    where
        elegir i =
            let x = xs !! i
                resto = drop (i + 1) xs
            in map (x:) (prefijos resto)









