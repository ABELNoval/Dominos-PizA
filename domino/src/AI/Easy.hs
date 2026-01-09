module AI.Easy
  ( chooseEasyMove
  ) where

import Game.GameState (GameState, jugadorActual, getTablero, getPozo)
import Game.Player (playerHand)
import Game.Rules (jugadasPosibles)
import Game.Actions (Accion(..))
import Game.Domino (esDoble, puntos)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

-- | IA Fácil: 
-- 1. Prioriza jugar dobles de mayor peso (suma de puntos)
-- 2. Si no tiene dobles, juega cualquier ficha válida
-- 3. Si no puede jugar, roba si hay pozo
-- 4. Si no hay pozo, pasa
chooseEasyMove :: GameState -> Accion
chooseEasyMove gs =
  let jugador = jugadorActual gs
      mano = playerHand jugador
      tablero = getTablero gs
      jugadas = jugadasPosibles mano tablero
      pozo = getPozo gs
      
      -- Separar jugadas con dobles y sin dobles
      jugadasDobles = filter (esDoble . fst) jugadas
      jugadasNormales = filter (not . esDoble . fst) jugadas
      
      -- Ordenar dobles por peso descendente
      doblesOrdenados = sortBy (comparing (Down . puntos . fst)) jugadasDobles
      -- Ordenar normales por peso descendente también
      normalesOrdenadas = sortBy (comparing (Down . puntos . fst)) jugadasNormales
      
  in case doblesOrdenados of
       -- Si hay dobles, jugar el de mayor peso
       ((d, l):_) -> Jugar d l
       [] -> case normalesOrdenadas of
               -- Si no hay dobles, jugar la ficha de mayor peso
               ((d, l):_) -> Jugar d l
               -- Si no hay jugadas posibles, robar o pasar
               [] -> if null pozo then Pasar else Robar
