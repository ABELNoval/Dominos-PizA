module Main (main) where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.IO (hSetEncoding, stdout, utf8)

-- =========================
-- ESTADO DEL MUNDO (GLOSS)
-- =========================

data World = World
  { worldJugadores :: [Jugador]
  , worldPozo      :: [Ficha]
  }

-- =========================
-- CREACIÓN DEL ESTADO INICIAL
-- =========================

estadoInicial :: IO World
estadoInicial = do
    hSetEncoding stdout utf8  -- opcional ahora, ya no imprimimos
    let fichas = generarFichas
    fichasBarajadas <- barajarFichas fichas

    let nJugadores = 4
    let (jugadores, pozo) = repartirManos fichasBarajadas nJugadores

    pure $ World
      { worldJugadores = jugadores
      , worldPozo = pozo
      }

-- =========================
-- MAIN
-- =========================

main :: IO ()
main = do
    world <- estadoInicial
    play
      (InWindow "Domino" (1100, 700) (100, 100))
      white
      60
      world
      drawWorld
      handleEvent
      updateWorld

-- =========================
-- DIBUJO
-- =========================

drawWorld :: World -> Picture
drawWorld world =
    Pictures $
        drawJugadores (worldJugadores world)
        ++ [drawPozo (worldPozo world)]

-- Texto helper
drawTexto :: Float -> Float -> String -> Picture
drawTexto x y str =
    Translate x y $
    Scale 0.15 0.15 $
    Text str

-- Dibujar jugadores
drawJugadores :: [Jugador] -> [Picture]
drawJugadores jugadores =
    zipWith drawJugador jugadores [0..]

drawJugador :: Jugador -> Int -> Picture
drawJugador jugador i =
    drawTexto (-500) (300 - fromIntegral i * 80) $
        "Jugador " ++ show (jugadorId jugador)
        ++ ": " ++ show (mano jugador)

-- Dibujar pozo
drawPozo :: [Ficha] -> Picture
drawPozo pozo =
    drawTexto (-500) (-300) $
        "Pozo: " ++ show pozo

-- =========================
-- EVENTOS (por ahora vacíos)
-- =========================

handleEvent :: Event -> World -> World
handleEvent _ world = world

-- =========================
-- ACTUALIZACIÓN (por ahora vacía)
-- =========================

updateWorld :: Float -> World -> World
updateWorld _ world = world
