module Game.Board
  ( Board(..)
  , Lado(..)
  , Jugada(..)
  , boardVacio
  , extremoIzquierdo
  , extremoDerecho
  , colocarFicha
  , fichasEnMesa
  , esTableroVacio
  ) where

import Game.Domino (Domino(..), voltear)
-- | Lado del tablero donde se puede colocar una ficha.
data Lado = Izquierda | Derecha
  deriving (Show, Eq)

-- | Una jugada: qué ficha se colocó y en qué lado.
data Jugada = Jugada
  { jugadaFicha :: Domino
  , jugadaLado  :: Lado
  } deriving (Show, Eq)

-- | El tablero es una lista de fichas colocadas en orden (de izquierda a derecha).
-- También guardamos los extremos abiertos para consulta rápida.
data Board = Board
  { fichasColocadas   :: [Domino]   -- fichas en la mesa, en orden
  , extremoIzq        :: Maybe Int  -- extremo abierto a la izquierda
  , extremoDer        :: Maybe Int  -- extremo abierto a la derecha
  } deriving (Show, Eq)

-- | Tablero vacío inicial.
boardVacio :: Board
boardVacio = Board
  { fichasColocadas = []
  , extremoIzq      = Nothing
  , extremoDer      = Nothing
  }

-- | Consultar el extremo izquierdo abierto.
extremoIzquierdo :: Board -> Maybe Int
extremoIzquierdo = extremoIzq

-- | Consultar el extremo derecho abierto.
extremoDerecho :: Board -> Maybe Int
extremoDerecho = extremoDer

-- | ¿El tablero está vacío?
esTableroVacio :: Board -> Bool
esTableroVacio board = null (fichasColocadas board)

-- | Fichas actualmente en la mesa.
fichasEnMesa :: Board -> [Domino]
fichasEnMesa = fichasColocadas

-- | Colocar una ficha en el tablero.
-- Devuelve Nothing si la jugada no es válida.
-- La ficha se orienta automáticamente para que conecte correctamente.
colocarFicha :: Domino -> Lado -> Board -> Maybe Board
colocarFicha ficha lado board
  | esTableroVacio board = Just $ colocarPrimera ficha board
  | otherwise            = colocarEnLado ficha lado board

-- | Colocar la primera ficha (caso especial: cualquier ficha vale).
colocarPrimera :: Domino -> Board -> Board
colocarPrimera ficha@(Domino a b) _ = Board
  { fichasColocadas = [ficha]
  , extremoIzq      = Just a
  , extremoDer      = Just b
  }

-- | Colocar ficha en un lado específico del tablero.
colocarEnLado :: Domino -> Lado -> Board -> Maybe Board
colocarEnLado ficha Izquierda board = colocarIzquierda ficha board
colocarEnLado ficha Derecha   board = colocarDerecha   ficha board

-- | Colocar ficha por la izquierda.
colocarIzquierda :: Domino -> Board -> Maybe Board
colocarIzquierda ficha@(Domino a b) board =
  case extremoIzq board of
    Nothing  -> Nothing
    Just ext
      | b == ext  -> Just board
          { fichasColocadas = ficha : fichasColocadas board
          , extremoIzq      = Just a
          }
      | a == ext  -> Just board
          { fichasColocadas = voltear ficha : fichasColocadas board
          , extremoIzq      = Just b
          }
      | otherwise -> Nothing

-- | Colocar ficha por la derecha.
colocarDerecha :: Domino -> Board -> Maybe Board
colocarDerecha ficha@(Domino a b) board =
  case extremoDer board of
    Nothing  -> Nothing
    Just ext
      | a == ext  -> Just board
          { fichasColocadas = fichasColocadas board ++ [ficha]
          , extremoDer      = Just b
          }
      | b == ext  -> Just board
          { fichasColocadas = fichasColocadas board ++ [voltear ficha]
          , extremoDer      = Just a
          }
      | otherwise -> Nothing
