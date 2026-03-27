import Prelude hiding (sum, elem, (++))

sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem e = foldr (\x acc -> x == e || acc) False

(++) :: [a] -> [a] -> [a]
(++) x1 x2 = foldr (:) x2 x1

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x acc -> if f x then x : acc else acc) []

map :: (a -> b) -> [a] -> [b]
-- map f = foldr (\x acc -> f x : acc) []
map f = foldr ((:) . f) []

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun _ [a] = a
mejorSegun f (x1:x2:xs) = if f x1 x2 then mejorSegun f (x1:xs) else mejorSegun f (x2:xs)

sumasParciales :: Num a => [a] -> [a]
sumasParciales xs = reverse (snd (foldl generar (0, []) xs))
    where
        generar (sumaAcc, listaAcc) x = 
            let nuevaSuma = sumaAcc + x
            in (nuevaSuma, nuevaSuma : listaAcc)

sumaAlt :: Num a => [a] -> a
sumaAlt xs = snd (foldr generar (odd (length xs), 0) xs)
    where 
        generar x (op, sumaAcc) =
            let nuevaSuma = if op then sumaAcc + x else sumaAcc - x
            in (not op, nuevaSuma)

sumaAltRev :: Num a => [a] -> a
sumaAltRev xs = snd (foldr generar (True, 0) xs)
    where 
        generar x (op, sumaAcc) =
            let nuevaSuma = if op then sumaAcc + x else sumaAcc - x
            in (not op, nuevaSuma)