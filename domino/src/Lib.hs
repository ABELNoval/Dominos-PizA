module Lib
    ( Ficha,
      generarFichas,
      barajarFichas,
      repartirManos,
      Mano,
      Jugador
    ) where

import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

-- | Representa una ficha de dominó con dos valores.
data Ficha = Ficha Int Int deriving (Show, Eq)

-- | Define un Jugador con un ID y su mano de fichas.
type Mano = [Ficha]
data Jugador = Jugador { jugadorId :: Int, mano :: Mano } deriving (Show)

-- | Genera el juego completo de fichas de dominó doble-nueve.
generarFichas :: [Ficha]
generarFichas = [Ficha i j | i <- [0..9], j <- [i..9]]

-- | Baraja una lista de fichas de forma aleatoria.
-- Devuelve una nueva lista de fichas barajadas y el nuevo generador de números aleatorios.
barajarFichas :: [Ficha] -> IO [Ficha]
barajarFichas fichas = do
    gen <- newStdGen
    return $ shuffle' fichas (length fichas) gen

-- | Reparte las fichas barajadas entre un número dado de jugadores.
-- Por ahora, asume un reparto fijo de 10 fichas por jugador.
repartirManos :: [Ficha] -> Int -> ([Jugador], [Ficha])
repartirManos fichas nJugadores =
    let (manos, resto) = splitAt (nJugadores * 10) fichas
        jugadores = zipWith (\id mano -> Jugador id mano) [1..nJugadores] (chunksOf 10 manos)
    in (jugadores, resto)

-- | Función auxiliar para dividir una lista en trozos de un tamaño dado.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
