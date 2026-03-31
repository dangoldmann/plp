data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB z _ Nil = z
foldAB z f (Bin i r d) = f (foldAB z f i) r (foldAB z f d)

recAB :: b -> (AB a -> b -> a -> AB a -> b -> b) -> AB a -> b
recAB z _ Nil = z
recAB z f (Bin i r d) = f i (recAB z f i) r d (recAB z f d)

esNil :: AB a -> Bool
esNil t = case t of 
  Nil  -> True 
  Bin {} -> False

altura :: AB a -> Integer
altura = foldAB 0 (\ri _ rd -> 1 + max ri rd)

cantNodos :: AB a -> Integer
cantNodos = foldAB 0 (\ri _ rd -> 1 + ri + rd)

mejorSegún :: (a -> a -> Bool) -> AB a -> a
mejorSegún f t = case foldAB Nothing paso t of
                   Just x  -> x
                   Nothing -> error "árbol vacío"
  where
    paso ri x rd = Just (mejorCon f (mejorCon f x ri) rd)

mejorCon :: (a -> a -> Bool) -> a -> Maybe a -> a
mejorCon _ x Nothing  = x
mejorCon f x (Just y) = if f x y then x else y

esABB :: Ord a => AB a -> Bool
esABB t =
  case foldAB Nothing paso t of
    Nothing         -> True
    Just (ok, _, _) -> ok
  where
    paso :: Ord a
         => Maybe (Bool, a, a)  -- info subárbol izquierdo
         -> a                   -- valor del nodo
         -> Maybe (Bool, a, a)  -- info subárbol derecho
         -> Maybe (Bool, a, a)
    paso izq x der = Just (ok, minimo, maximo)
      where
        okIzq = case izq of
          Nothing          -> True
          Just (b, _, mxI) -> b && mxI <= x

        okDer = case der of
          Nothing          -> True
          Just (b, mnD, _) -> b && x < mnD

        ok = okIzq && okDer

        minimo = case izq of
          Nothing        -> x
          Just (_, mn,_) -> mn

        maximo = case der of
          Nothing        -> x
          Just (_, _,mx) -> mx