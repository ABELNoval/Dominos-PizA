module Main (main) where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.IO (hSetEncoding, stdout, utf8)
import System.Environment (getArgs)
import qualified Game.GameState as GS
import qualified Game.Player as GP
import qualified Game.Board as GB
import qualified Game.Actions as GA
import qualified Game.Rules as GR
import Control.Monad (when)
import Data.Char (toLower)

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
        args <- getArgs
        case args of
            (m:_) | map toLower m == "cli" -> terminalMain
            _ -> do
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
-- MODO TERMINAL (CLI)
-- =========================

terminalMain :: IO ()
terminalMain = do
    putStrLn "Iniciando partida en modo CLI (4 jugadores controlados manualmente)."
    gs <- GS.iniciarPartida ["Jugador 1","Jugador 2","Jugador 3","Jugador 4"] 10
    cliLoop gs

cliLoop :: GS.GameState -> IO ()
cliLoop gs = do
    if GS.estaTerminado gs
        then mostrarResultadoFinal gs
        else do
            mostrarEstado gs
            let acciones = GA.accionesDisponibles gs
            putStrLn "Acciones disponibles:"
            imprimirAcciones acciones
            pedirYProcesarAccion gs acciones

-- Mostrar estado del juego (mesa y manos)
mostrarEstado :: GS.GameState -> IO ()
mostrarEstado gs = do
    let tablero   = GS.getTablero gs
        jugadores = GS.getJugadores gs
        jugador   = GS.jugadorActual gs
    putStrLn "\n================ TURNO ================="
    putStrLn $ "Turno de: " ++ GP.playerName jugador ++ " (id=" ++ show (GP.playerId jugador) ++ ")"
    putStrLn $ "Mesa: " ++ show (GB.fichasEnMesa tablero)
    putStrLn $ "Extremo Izq: " ++ show (GB.extremoIzquierdo tablero) ++ ", Extremo Der: " ++ show (GB.extremoDerecho tablero)
    putStrLn "Mano actual:"
    imprimirMano (GP.playerHand jugador)

imprimirMano :: GP.Hand -> IO ()
imprimirMano mano = do
    let enumerada = zip [0..] mano
    mapM_ (\(i, f) -> putStrLn $ "  [" ++ show i ++ "] " ++ show f) enumerada

imprimirAcciones :: [GA.Accion] -> IO ()
imprimirAcciones acciones = do
    let mostrar (GA.Jugar ficha lado) = "j " ++ fichaStr ficha ++ " " ++ ladoStr lado
        mostrar GA.Pasar              = "p (pasar)"
        mostrar GA.Robar              = "r (robar)"
    mapM_ (putStrLn . ("  " ++) . mostrar) acciones
    putStrLn "Formato de entrada:"
    putStrLn "  j <indice> <l|r>  - jugar la ficha por izquierda o derecha"
    putStrLn "  r                 - robar del pozo"
    putStrLn "  p                 - pasar (solo si no hay pozo y no puedes jugar)"
    where
        fichaStr f = show f
        ladoStr GB.Izquierda = "l"
        ladoStr GB.Derecha   = "r"

pedirYProcesarAccion :: GS.GameState -> [GA.Accion] -> IO ()
pedirYProcesarAccion gs _ = do
    putStr "> "
    inp <- getLine
    case words (map toLower inp) of
        ["j", idxStr, ladoStr] ->
            case (reads idxStr :: [(Int,String)], parseLado ladoStr) of
                ([(idx, _)], Just lado) ->
                    let jugador = GS.jugadorActual gs
                        mano    = GP.playerHand jugador
                    in if idx >= 0 && idx < length mano
                         then procesarResultado gs (GA.ejecutarAccion (GA.Jugar (mano !! idx) lado) gs)
                         else do
                             putStrLn "Índice fuera de rango."
                             cliLoop gs
                _ -> do
                    putStrLn "Entrada inválida. Usa: j <indice> <l|r>"
                    cliLoop gs
        ["r"] -> procesarResultado gs (GA.ejecutarAccion GA.Robar gs)
        ["p"] -> procesarResultado gs (GA.ejecutarAccion GA.Pasar gs)
        ["q"] -> putStrLn "Saliendo..."
        _      -> do
            putStrLn "Comando no reconocido. Usa j/r/p o q para salir."
            cliLoop gs

procesarResultado :: GS.GameState -> GA.ResultadoAccion -> IO ()
procesarResultado gsOriginal res = case res of
    GA.Exito gs' -> cliLoop gs'
    GA.Victoria jugador gs' -> do
        putStrLn $ "\n¡Victoria! " ++ GP.playerName jugador ++ " se quedó sin fichas."
        mostrarEstado gs'
        putStrLn "Partida terminada."
    GA.Trancado resultado gs' -> do
        putStrLn "\nPartida trancada. Resultado:"
        imprimirResultado resultado
        mostrarEstado gs'
        putStrLn "Partida terminada."
    GA.ErrorAccion msg -> do
        putStrLn $ "Error: " ++ msg
        putStrLn "Intenta otra acción."
        cliLoop gsOriginal  -- vuelve al loop con el estado sin cambios

mostrarResultadoFinal :: GS.GameState -> IO ()
mostrarResultadoFinal gs = do
    let jugadores = GS.getJugadores gs
        tablero   = GS.getTablero gs
    if GR.estaTrancado jugadores tablero
        then imprimirResultado (GR.determinarGanador jugadores Nothing)
        else putStrLn "La partida ya marcó ganador anteriormente."

imprimirResultado :: GR.ResultadoPartida -> IO ()
imprimirResultado res = case res of
    GR.GanadorDomino j   -> putStrLn $ "Ganador por domino: " ++ GP.playerName j
    GR.GanadorTrancado j -> putStrLn $ "Ganador por trancado (menos puntos): " ++ GP.playerName j
    GR.Empate js         -> putStrLn $ "Empate entre: " ++ show (map GP.playerName js)

parseLado :: String -> Maybe GB.Lado
parseLado s = case s of
    "l" -> Just GB.Izquierda
    "r" -> Just GB.Derecha
    _    -> Nothing

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
