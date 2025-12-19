module Lib
    ( Ficha(..)
    , generarFichas
    , barajarFichas
    , repartirManos
    , Mano
    , Jugador(..)
    ) where

import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

-- | Representa una ficha de dominó con dos valores.
data Ficha = Ficha
  { ladoA :: Int
  , ladoB :: Int
  } deriving (Show, Eq)

-- | Mano de fichas
type Mano = [Ficha]

-- | Define un Jugador con un ID y su mano de fichas.
data Jugador = Jugador
  { jugadorId :: Int
  , mano      :: Mano
  } deriving (Show)

-- | Genera el juego completo de fichas de dominó doble-nueve.
generarFichas :: [Ficha]
generarFichas =
  [Ficha i j | i <- [0..9], j <- [i..9]]

-- | Baraja una lista de fichas de forma aleatoria.
barajarFichas :: [Ficha] -> IO [Ficha]
barajarFichas fichas = do
    gen <- newStdGen
    pure $ shuffle' fichas (length fichas) gen

-- | Reparte las fichas barajadas entre un número dado de jugadores.
-- Por ahora, asume un reparto fijo de 10 fichas por jugador.
repartirManos :: [Ficha] -> Int -> ([Jugador], [Ficha])
repartirManos fichas nJugadores =
    let (manos, resto) = splitAt (nJugadores * 10) fichas
        jugadores =
          zipWith
            (\jid m -> Jugador jid m)
            [1..nJugadores]
            (chunksOf 10 manos)
    in (jugadores, resto)

-- | Función auxiliar para dividir una lista en trozos de un tamaño dado.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
