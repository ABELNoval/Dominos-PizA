module Game.Player
  ( Player(..)
  , Hand
  , mkPlayer
  , tieneFichas
  , cantidadFichas
  , quitarFicha
  , agregarFicha
  , puntosEnMano
  , tieneFicha
  ) where

import Game.Domino (Domino, puntos)

-- | Mano de un jugador: lista de fichas.
type Hand = [Domino]

-- | Un jugador con identificador, nombre y mano de fichas.
data Player = Player
  { playerId   :: Int
  , playerName :: String
  , playerHand :: Hand
  } deriving (Show, Eq)

-- | Crear un jugador con mano vacía.
mkPlayer :: Int -> String -> Player
mkPlayer pid name = Player
  { playerId   = pid
  , playerName = name
  , playerHand = []
  }

-- | ¿El jugador tiene fichas en la mano?
tieneFichas :: Player -> Bool
tieneFichas = not . null . playerHand

-- | Cantidad de fichas en la mano.
cantidadFichas :: Player -> Int
cantidadFichas = length . playerHand

-- | Quitar una ficha de la mano del jugador.
-- Devuelve Nothing si el jugador no tiene esa ficha.
quitarFicha :: Domino -> Player -> Maybe Player
quitarFicha ficha player =
  if ficha `elem` playerHand player
    then Just player { playerHand = remove ficha (playerHand player) }
    else Nothing
  where
    remove _ []     = []
    remove x (y:ys)
      | x == y    = ys
      | otherwise = y : remove x ys

-- | Agregar una ficha a la mano del jugador (al robar del pozo).
agregarFicha :: Domino -> Player -> Player
agregarFicha ficha player =
  player { playerHand = ficha : playerHand player }

-- | Total de puntos en la mano del jugador.
puntosEnMano :: Player -> Int
puntosEnMano = sum . map puntos . playerHand

-- | ¿El jugador tiene esta ficha en la mano?
tieneFicha :: Domino -> Player -> Bool
tieneFicha ficha player = ficha `elem` playerHand player
