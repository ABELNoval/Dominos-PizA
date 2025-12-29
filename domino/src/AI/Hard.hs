module AI.Hard
  ( chooseHardMove
  ) where

import Game.GameState (GameState, jugadorActual, getTablero, getPozo)
import Game.Player (playerHand)
import Game.Rules (jugadasPosibles)
import Game.Actions (Accion(..))

-- | IA Difícil (Placeholder):
-- Por ahora funciona igual que Medium
-- En el futuro incluirá:
-- - Conteo de fichas jugadas
-- - Predicción de manos de oponentes
-- - Estrategias avanzadas de bloqueo
-- - Análisis de pases de otros jugadores
chooseHardMove :: GameState -> Accion
chooseHardMove gs =
  let jugador = jugadorActual gs
      mano = playerHand jugador
      tablero = getTablero gs
      jugadas = jugadasPosibles mano tablero
      pozo = getPozo gs
  in case jugadas of
       ((d, l):_) -> Jugar d l
       [] -> if null pozo then Pasar else Robar
