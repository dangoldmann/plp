data AB a = Nil | Bin (AB a) a (AB a) deriving Show

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin i v d) = Bin (negacionAB i) (not v) (negacionAB d)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin i v d) = productoAB i * v * productoAB d