import Prelude hiding (subtract)

-- max2 :: Ord a => (a, a) -> a
-- normaVectorial :: Floating a => (a, a) -> a
-- subtract :: Num a => a -> a -> a
-- predecesor :: Num a => a -> a
-- evaluarEnCero :: Num a => (a -> b) -> b
-- dosVeces :: (a -> a) -> (a -> a)
-- flipAll :: [a -> b -> c] -> [b -> a -> c]
-- flipRaro :: (a -> b -> c) -> (a -> b -> c)

-- No currificadas: max2, normaVectorial 
max2Currificada :: Ord a => a -> a -> a
max2Currificada = max

normaVectorial :: Floating a => a -> a -> a
normaVectorial x y = sqrt (x^2 + y^2)