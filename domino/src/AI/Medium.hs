module AI.Medium
  ( chooseMediumMove
  ) where

import Game.GameState (GameState, jugadorActual, getTablero, getPozo)
import Game.Player (playerHand)
import Game.Rules (jugadasPosibles)
import Game.Actions (Accion(..))
import Game.Domino (Domino(..), esDoble, puntos, extremos)
import Game.Board (Board, Lado(..), extremoIzquierdo, extremoDerecho, esTableroVacio)
import Data.List (sortBy, groupBy, maximumBy)
import Data.Ord (comparing, Down(..))
import Data.Function (on)
import qualified Data.Map as Map

-- | IA Media:
-- 1. Cuenta la frecuencia de cada número en la mano
-- 2. Prioriza jugadas que NO cubran los números más frecuentes
-- 3. Es decir, intenta mantener en el tablero los extremos que coincidan con
--    los números que más tiene en la mano
-- 4. Si tiene dobles de números muy frecuentes, los juega para no quedarse trabado
chooseMediumMove :: GameState -> Accion
chooseMediumMove gs =
  let jugador = jugadorActual gs
      mano = playerHand jugador
      tablero = getTablero gs
      jugadas = jugadasPosibles mano tablero
      pozo = getPozo gs
      
  in if null jugadas
       then if null pozo then Pasar else Robar
       else 
         let -- Contar frecuencia de cada número en la mano
             frecuencias = contarFrecuencias mano
             
             -- Obtener extremos actuales del tablero
             extIzq = extremoIzquierdo tablero
             extDer = extremoDerecho tablero
             
             -- Puntuar cada jugada
             jugadasPuntuadas = map (puntuarJugada frecuencias extIzq extDer) jugadas
             
             -- Ordenar por puntuación descendente
             jugadasOrdenadas = sortBy (comparing (Down . snd)) jugadasPuntuadas
             
             -- Tomar la mejor jugada
             ((mejorFicha, mejorLado), _) = head jugadasOrdenadas
             
         in Jugar mejorFicha mejorLado

-- | Contar cuántas veces aparece cada número en la mano
contarFrecuencias :: [Domino] -> Map.Map Int Int
contarFrecuencias mano =
  let numeros = concatMap (\d -> let (a, b) = extremos d in [a, b]) mano
  in foldr (\n acc -> Map.insertWith (+) n 1 acc) Map.empty numeros

-- | Puntuar una jugada basándose en la estrategia de frecuencias
-- Mayor puntuación = mejor jugada
puntuarJugada :: Map.Map Int Int -> Maybe Int -> Maybe Int -> (Domino, Lado) -> ((Domino, Lado), Int)
puntuarJugada frecuencias extIzq extDer (ficha, lado) =
  let (a, b) = extremos ficha
      puntoBase = puntos ficha  -- Base: fichas más pesadas dan más puntos
      
      -- Obtener el extremo que se va a cubrir
      extremoCubierto = case lado of
        Izquierda -> extIzq
        Derecha -> extDer
      
      -- Determinar qué número quedará expuesto
      numeroExpuesto = case extremoCubierto of
        Nothing -> 0  -- Tablero vacío
        Just ext -> if a == ext then b else a
      
      -- Frecuencia del número que quedará expuesto
      freqExpuesto = Map.findWithDefault 0 numeroExpuesto frecuencias
      
      -- Frecuencia del extremo que se cubre (queremos evitar cubrir extremos con alta frecuencia)
      freqCubierto = case extremoCubierto of
        Nothing -> 0
        Just ext -> Map.findWithDefault 0 ext frecuencias
      
      -- Bonus por dejar expuesto un número frecuente
      bonusExpuesto = freqExpuesto * 15
      
      -- Penalización por cubrir un extremo con número frecuente
      -- Solo penalizar si ese extremo tiene el número que queremos conservar
      penalizacionCubrir = freqCubierto * 5
      
      -- Bonus para dobles: jugarlos pronto para evitar quedarse atrapado
      bonusDoble = if esDoble ficha then 20 else 0
      
      -- Puntuación final
      puntuacion = puntoBase + bonusExpuesto + bonusDoble - penalizacionCubrir
      
  in ((ficha, lado), puntuacion)
