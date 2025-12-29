module Game.Actions
  ( Accion(..)
  , ResultadoAccion(..)
  , ejecutarAccion
  , accionesDisponibles
  ) where

import Game.Domino (Domino)
import Game.Board (Lado(..), colocarFicha)
import Game.Player (Player(..), quitarFicha, agregarFicha, tieneFichas)
import Game.GameState
    ( GameState(..)
    , FaseJuego(..)
    , jugadorActual
    , siguienteTurno
    , actualizarJugador
    , actualizarTablero
    , sacarDelPozo
    , getTablero
    , getJugadores
    , getPozo
    )
import Game.Rules (jugadasPosibles, puedeJugar, estaTrancado, determinarGanador, ResultadoPartida(..))

-- | Acciones posibles en el juego.
data Accion
  = Jugar Domino Lado   -- Jugar una ficha en un lado del tablero
  | Pasar               -- Pasar turno (cuando no se puede jugar y el pozo está vacío)
  | Robar               -- Robar una ficha del pozo
  deriving (Show, Eq)

-- | Resultado de ejecutar una acción.
data ResultadoAccion
  = Exito GameState             -- Acción exitosa, nuevo estado
  | Victoria Player GameState   -- Un jugador ganó (se quedó sin fichas)
  | Trancado ResultadoPartida GameState  -- Juego trancado
  | ErrorAccion String          -- Acción inválida
  deriving (Show)

-- | Ejecutar una acción y obtener el nuevo estado.
ejecutarAccion :: Accion -> GameState -> ResultadoAccion
ejecutarAccion accion gs
  | gsFase gs /= EnCurso = ErrorAccion "La partida no está en curso"
  | otherwise            = case accion of
      Jugar ficha lado -> ejecutarJugar ficha lado gs
      Pasar            -> ejecutarPasar gs
      Robar            -> ejecutarRobar gs

-- | Ejecutar la acción de jugar una ficha.
ejecutarJugar :: Domino -> Lado -> GameState -> ResultadoAccion
ejecutarJugar ficha lado gs =
    let jugador = jugadorActual gs
        tablero = getTablero gs
    in case quitarFicha ficha jugador of
         Nothing -> ErrorAccion "El jugador no tiene esa ficha"
         Just jugadorSinFicha ->
           case colocarFicha ficha lado tablero of
             Nothing -> ErrorAccion "Jugada inválida: la ficha no conecta"
             Just nuevoTablero ->
               let gs1 = actualizarJugador jugadorSinFicha gs
                   gs2 = actualizarTablero nuevoTablero gs1
                   gs3 = gs2 { gsPasesConsec = 0 }  -- Resetear pases
               in verificarFinPartida jugadorSinFicha gs3

-- | Verificar si la partida terminó después de una jugada.
verificarFinPartida :: Player -> GameState -> ResultadoAccion
verificarFinPartida jugador gs
  | not (tieneFichas jugador) =
      -- ¡El jugador ganó! Se quedó sin fichas
      let gsFinal = gs { gsFase = Terminado }
      in Victoria jugador gsFinal
  | estaTrancado (getJugadores gs) (getTablero gs) =
      -- Juego trancado
      let resultado = determinarGanador (getJugadores gs) Nothing
          gsFinal   = gs { gsFase = Terminado }
      in Trancado resultado gsFinal
  | otherwise =
      -- Continúa el juego
      Exito (siguienteTurno gs)

-- | Ejecutar la acción de pasar.
ejecutarPasar :: GameState -> ResultadoAccion
ejecutarPasar gs =
    let jugador = jugadorActual gs
        tablero = getTablero gs
        pozo    = getPozo gs
    in if puedeJugar (playerHand jugador) tablero
         then ErrorAccion "No puedes pasar si tienes jugadas disponibles"
         else if not (null pozo)
           then ErrorAccion "No puedes pasar si hay fichas en el pozo (debes robar)"
           else
             let nuevosPases = gsPasesConsec gs + 1
                 nJugadores  = length (getJugadores gs)
                 gs1 = gs { gsPasesConsec = nuevosPases }
             in if nuevosPases >= nJugadores
                  then -- Todos pasaron, partida trancada
                    let resultado = determinarGanador (getJugadores gs) Nothing
                        gsFinal   = gs1 { gsFase = Terminado }
                    in Trancado resultado gsFinal
                  else Exito (siguienteTurno gs1)

-- | Ejecutar la acción de robar del pozo.
ejecutarRobar :: GameState -> ResultadoAccion
ejecutarRobar gs =
    let jugador = jugadorActual gs
        tablero = getTablero gs
    in if puedeJugar (playerHand jugador) tablero
         then ErrorAccion "No puedes robar si tienes jugadas disponibles"
         else case sacarDelPozo gs of
           (Nothing, _)      -> ErrorAccion "El pozo está vacío"
           (Just ficha, gs1) ->
             let jugadorConFicha = agregarFicha ficha jugador
                 gs2 = actualizarJugador jugadorConFicha gs1
             -- Después de robar, el turno NO pasa automáticamente.
             -- El jugador debe intentar jugar o seguir robando.
             in Exito gs2

-- | Obtener las acciones disponibles para el jugador actual.
accionesDisponibles :: GameState -> [Accion]
accionesDisponibles gs =
    let jugador = jugadorActual gs
        tablero = getTablero gs
        pozo    = getPozo gs
        jugadas = jugadasPosibles (playerHand jugador) tablero
    in if not (null jugadas)
         then [Jugar ficha lado | (ficha, lado) <- jugadas]
         else if not (null pozo)
           then [Robar]
           else [Pasar]
