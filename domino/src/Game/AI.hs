module Game.AI
  ( chooseBotAccion
  ) where

import Game.GameState (GameState, jugadorActual, getTablero, getPozo)
import Game.Player (playerHand)
import Game.Rules (jugadasPosibles)
import Game.Actions (Accion(..))

-- | Elegir una acción para el bot: primera jugada posible; si no hay, robar si hay pozo; si no, pasar.
chooseBotAccion :: GameState -> Accion
chooseBotAccion gs =
  let jugador = jugadorActual gs
      mano = playerHand jugador
      tablero = getTablero gs
      jugadas = jugadasPosibles mano tablero
      pozo = getPozo gs
  in case jugadas of
       ((d, l):_) -> Jugar d l
       [] -> if null pozo then Pasar else Robar
