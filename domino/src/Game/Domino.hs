module Game.Domino
  ( Domino(..)
  , mkDomino
  , extremos
  , voltear
  , esDoble
  , puntos
  , puedeConectar
  , generarFichasCompletas
  ) where

-- | Una ficha de dominó con dos extremos (0-9 en dominó doble-nueve).
data Domino = Domino
  { extremoA :: Int
  , extremoB :: Int
  } deriving (Eq)

instance Show Domino where
  show (Domino a b) = "[" ++ show a ++ "|" ++ show b ++ "]"

-- | Constructor seguro: normaliza para que extremoA <= extremoB.
mkDomino :: Int -> Int -> Domino
mkDomino a b
  | a <= b    = Domino a b
  | otherwise = Domino b a

-- | Devuelve los dos extremos como tupla.
extremos :: Domino -> (Int, Int)
extremos (Domino a b) = (a, b)

-- | Voltea la ficha (intercambia extremos).
voltear :: Domino -> Domino
voltear (Domino a b) = Domino b a

-- | ¿Es una ficha doble? (ambos extremos iguales)
esDoble :: Domino -> Bool
esDoble (Domino a b) = a == b

-- | Puntos de la ficha (suma de ambos extremos).
puntos :: Domino -> Int
puntos (Domino a b) = a + b

-- | ¿Puede esta ficha conectar con un valor dado?
puedeConectar :: Int -> Domino -> Bool
puedeConectar valor (Domino a b) = a == valor || b == valor

-- | Genera el juego completo de fichas de dominó doble-nueve (55 fichas).
generarFichasCompletas :: [Domino]
generarFichasCompletas =
  [ Domino i j | i <- [0..9], j <- [i..9] ]
