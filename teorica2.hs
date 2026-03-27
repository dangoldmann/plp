import Prelude hiding (foldr, foldl, (.))

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

foldrWithRecr :: (a -> b -> b) -> b -> [a] -> b
foldrWithRecr f = recr (\ x _ rec -> f x rec)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z [] = z
foldr f z (x : xs) = f x (foldr f z xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl f acc (x : xs) = foldl f (f acc x) xs

recrWithFoldr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recrWithFoldr f z xs = foldr step base xs xs
    where
        base [] = z
        step x rec (_:xs') = f x xs' (rec xs')

foldlWithFoldr :: (b -> a -> b) -> b -> [a] -> b
foldlWithFoldr f z xs = foldr step id xs z
    where
        step x g acc = g (f acc x)

foldrWithFoldl :: (a -> b -> b) -> b -> [a] -> b
foldrWithFoldl f z xs = foldl step id xs z
  where
    step g x acc = g (f x acc)

data Dia = Lun | Mar | Mie | Jue | Vie | Sab | Dom

esFinDeSemana :: Dia -> Bool
esFinDeSemana Sab = True
esFinDeSemana Dom = True
esFinDeSemana _ = False

data Persona = LaPersona String String Int

nombre :: Persona -> String
nombre (LaPersona n _ _ ) = n

apellido :: Persona -> String
apellido (LaPersona _ a _) = a

fechaNacimiento :: Persona -> Int
fechaNacimiento (LaPersona _ _ f) = f

danGoldman = LaPersona "Dan" "Goldman" 2005

data Forma = Rectangulo Float Float | Circulo Float

area :: Forma -> Float
area (Rectangulo ancho alto) = ancho * alto
area (Circulo radio) = radio * radio * pi

data Nat = Zero | Succ Nat

-- doble :: Nat -> Nat
-- doble Zero = Zero
-- doble (Succ n) = Succ (Succ (doble n))

data Lista a = Vacia | Cons a (Lista a)

data AB a = Nil | Bin (AB a) a (AB a)

insertar :: Ord a => a -> AB a -> AB a
insertar x Nil = Bin Nil x Nil
insertar x (Bin izq y der)
    | x < y = insertar x izq
    | x > y = insertar x der
    | otherwise = Bin izq y der

-- prod :: (Int, Int) -> Int
-- prod (x, y) = x * y

prod' :: Int -> (Int -> Int)
prod' x = \y -> x * y

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = f (x, y)
-- curry f = \x -> \y -> f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (x, y) = f x y
-- uncurry f = \ (x, y) -> f x y

triple :: Float -> Float
triple = (*) 3

esMayorDeEdad :: Int -> Bool
esMayorDeEdad e = (>=) e 18

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f1 f2 a = f1 (f2 a)

flip :: (a -> b -> c) -> b -> a -> c
flip f a b = f b a

($) :: (a -> b) -> a -> b
($) f = f

const :: a -> b -> a
const a _ = a

extension = [1,2,3,4]
secuencias = [3..7]
comprension = [(x, y) | x <- [0..5], y <- [0..3], x + y == 4]
naturales = [1..]
multiplosDe3 = [0,3..]
holas = repeat "hola"
-- primos = [n | n <- [2..], esPrimo n]
infinitosUnos = 1 : infinitosUnos

-- maximo :: Ord a => [a] -> a
-- maximo [a] = a
-- maximo (x1:x2:xs) = if x1 > x2 then maximo (x1:xs) else maximo (x2:xs)

-- listaMasCorta :: [[a]] -> [a]
-- listaMasCorta [a] = a
-- listaMasCorta (x1:x2:xs) = if length x1 < length x2 then listaMasCorta (x1:xs) else listaMasCorta (x2:xs)

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun _ [a] = a
mejorSegun f (x1:x2:xs) = if f x1 x2 then mejorSegun f (x1:xs) else mejorSegun f (x2:xs)

maximo :: Ord a => [a] -> a
maximo = mejorSegun (>)

listaMasCorta :: [[a]] -> [a]
listaMasCorta = mejorSegun (\ x y -> length x < length y)

deLongitudN :: Int -> [[a]] -> [[a]]
deLongitudN n = filter (\x -> length x == n)

soloPuntosFijosEnN :: Int -> [Int -> Int] -> [Int -> Int]
soloPuntosFijosEnN n = filter (\x -> x n == n)

reverseAnidado :: [[Char]] -> [[Char]]
reverseAnidado xs = reverse (map reverse xs)

paresCuadrados :: [Int] -> [Int]
paresCuadrados = map (\x -> if even x then x * x else x)

listaComp :: (a -> Bool) -> (a -> b) -> [a] -> [b]
-- listaComp p f xs = [f x | x <- xs, p x]
-- listaComp p f xs = map f (filter p xs)
listaComp p f = map f . filter p