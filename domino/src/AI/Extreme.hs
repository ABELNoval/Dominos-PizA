module AI.Extreme
  ( chooseExtremeMove
  ) where

import Game.GameState
import Game.Player
import Game.Rules
import Game.Actions
import Game.Domino
import Game.Board

import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, mapMaybe)

{-
  =========================================================
  IA EXTREMA - INFORMACIÓN PERFECTA
  =========================================================
  
  Esta IA tiene acceso completo a:
  - Todas las manos de todos los jugadores
  - El estado del tablero
  - Las fichas en el pozo
  
  Usa minimax con profundidad limitada para encontrar
  la mejor jugada considerando las respuestas óptimas
  de los oponentes.
-}

--------------------------------------------------------------------------------
-- CONFIGURACIÓN
--------------------------------------------------------------------------------

-- | Profundidad máxima del árbol de búsqueda
maxDepth :: Int
maxDepth = 4

--------------------------------------------------------------------------------
-- ENTRADA PRINCIPAL
--------------------------------------------------------------------------------

-- | Elegir la mejor jugada con información perfecta
chooseExtremeMove :: GameState -> Accion
chooseExtremeMove gs =
  let
    jugadorIdx = gsTurnoActual gs
    jugadores  = getJugadores gs
    jugador    = jugadores !! jugadorIdx
    mano       = playerHand jugador
    tablero    = getTablero gs
    pozo       = getPozo gs
    
    jugadas = jugadasPosibles mano tablero
  in case jugadas of
      [] -> if null pozo then Pasar else Robar
      _  ->
        let
          -- Evaluar cada jugada con minimax
          jugadasEvaluadas = map (evaluarConMinimax gs jugadorIdx maxDepth) jugadas
          -- Elegir la mejor
          mejor = fst $ maximumBy (comparing snd) jugadasEvaluadas
        in uncurry Jugar mejor

--------------------------------------------------------------------------------
-- MINIMAX CON INFORMACIÓN PERFECTA
--------------------------------------------------------------------------------

-- | Evaluar una jugada usando minimax
evaluarConMinimax :: GameState -> Int -> Int -> (Domino, Lado) -> ((Domino, Lado), Int)
evaluarConMinimax gs miIdx depth jugada@(ficha, lado) =
  let
    -- Simular la jugada
    estadoSimulado = simularJugada gs ficha lado
    -- Evaluar con minimax (el siguiente jugador es minimizador si es rival)
    score = minimax estadoSimulado miIdx (depth - 1) False
  in (jugada, score)

-- | Algoritmo minimax
-- miIdx: índice del jugador que maximiza
-- depth: profundidad restante
-- isMax: True si es turno del maximizador
minimax :: GameState -> Int -> Int -> Bool -> Int
minimax gs miIdx depth isMax
  | depth <= 0 = evaluarEstadoCompleto gs miIdx
  | gsFase gs == Terminado = evaluarEstadoFinal gs miIdx
  | otherwise =
      let
        turnoActual = gsTurnoActual gs
        jugadores = getJugadores gs
        jugador = jugadores !! turnoActual
        mano = playerHand jugador
        tablero = getTablero gs
        pozo = getPozo gs
        jugadas = jugadasPosibles mano tablero
        
        -- Determinar si este jugador es aliado (mismo equipo) o rival
        esAliado = esDelMismoEquipo miIdx turnoActual (length jugadores)
        
      in if null jugadas
         then
           -- Si no puede jugar, simular pasar o robar
           if null pozo
             then
               let nuevoGs = simularPasar gs
               in minimax nuevoGs miIdx (depth - 1) isMax
             else
               let nuevoGs = simularRobar gs
               in minimax nuevoGs miIdx (depth - 1) isMax
         else
           let
             -- Evaluar todas las jugadas posibles
             scores = map (\(f, l) ->
                            let gs' = simularJugada gs f l
                            in minimax gs' miIdx (depth - 1) (not isMax)
                          ) jugadas
           in if esAliado
              then maximum scores  -- Aliado maximiza
              else minimum scores  -- Rival minimiza

-- | Determinar si dos jugadores son del mismo equipo
esDelMismoEquipo :: Int -> Int -> Int -> Bool
esDelMismoEquipo idx1 idx2 nJugadores
  | nJugadores <= 2 = idx1 == idx2
  | otherwise = idx1 `mod` 2 == idx2 `mod` 2  -- En 2v2: 0,2 vs 1,3

--------------------------------------------------------------------------------
-- SIMULACIÓN DE JUGADAS
--------------------------------------------------------------------------------

-- | Simular una jugada y devolver el nuevo estado
simularJugada :: GameState -> Domino -> Lado -> GameState
simularJugada gs ficha lado =
  let
    turno = gsTurnoActual gs
    jugadores = gsJugadores gs
    jugador = jugadores !! turno
    tablero = gsTablero gs
    
    -- Quitar ficha de la mano
    nuevaMano = filter (/= ficha) (playerHand jugador)
    jugadorActualizado = jugador { playerHand = nuevaMano }
    jugadoresActualizados = actualizarEnLista turno jugadorActualizado jugadores
    
    -- Colocar ficha en tablero
    nuevoTablero = fromMaybe tablero (colocarFicha ficha lado tablero)
    
    -- Verificar si ganó
    nuevaFase = if null nuevaMano then Terminado else gsFase gs
    
    -- Siguiente turno
    nJugadores = length jugadores
    siguienteTurno' = (turno + 1) `mod` nJugadores
    
  in gs { gsJugadores = jugadoresActualizados
        , gsTablero = nuevoTablero
        , gsTurnoActual = siguienteTurno'
        , gsFase = nuevaFase
        , gsPasesConsec = 0
        }

-- | Simular pasar turno
simularPasar :: GameState -> GameState
simularPasar gs =
  let
    nJugadores = length (gsJugadores gs)
    nuevosPases = gsPasesConsec gs + 1
    siguienteTurno' = (gsTurnoActual gs + 1) `mod` nJugadores
    nuevaFase = if nuevosPases >= nJugadores then Terminado else gsFase gs
  in gs { gsTurnoActual = siguienteTurno'
        , gsPasesConsec = nuevosPases
        , gsFase = nuevaFase
        }

-- | Simular robar del pozo
simularRobar :: GameState -> GameState
simularRobar gs =
  case gsPozo gs of
    [] -> simularPasar gs
    (f:resto) ->
      let
        turno = gsTurnoActual gs
        jugadores = gsJugadores gs
        jugador = jugadores !! turno
        jugadorConFicha = jugador { playerHand = f : playerHand jugador }
        jugadoresActualizados = actualizarEnLista turno jugadorConFicha jugadores
      in gs { gsJugadores = jugadoresActualizados
            , gsPozo = resto
            }

-- | Actualizar elemento en una lista por índice
actualizarEnLista :: Int -> a -> [a] -> [a]
actualizarEnLista idx elem lista =
  take idx lista ++ [elem] ++ drop (idx + 1) lista

--------------------------------------------------------------------------------
-- EVALUACIÓN DE ESTADOS
--------------------------------------------------------------------------------

-- | Evaluar estado completo (heurística)
evaluarEstadoCompleto :: GameState -> Int -> Int
evaluarEstadoCompleto gs miIdx =
  let
    jugadores = gsJugadores gs
    nJugadores = length jugadores
    tablero = gsTablero gs
    
    -- Calcular puntuación basada en varios factores
    
    -- 1. Fichas restantes (menos es mejor)
    misFichas = length $ playerHand (jugadores !! miIdx)
    misPuntos = puntosEnMano (jugadores !! miIdx)
    
    -- 2. Opciones de juego disponibles
    misOpciones = length $ jugadasPosibles (playerHand (jugadores !! miIdx)) tablero
    
    -- 3. Ventaja sobre rivales
    (aliados, rivales) = particionarEquipos miIdx jugadores
    
    puntosAliados = sum $ map puntosEnMano aliados
    puntosRivales = sum $ map puntosEnMano rivales
    
    fichasAliados = sum $ map (length . playerHand) aliados
    fichasRivales = sum $ map (length . playerHand) rivales
    
    opcionesAliados = sum $ map (\p -> length $ jugadasPosibles (playerHand p) tablero) aliados
    opcionesRivales = sum $ map (\p -> length $ jugadasPosibles (playerHand p) tablero) rivales
    
    -- Puntuación final
    scoreFichas = (fichasRivales - fichasAliados) * 50
    scorePuntos = (puntosRivales - puntosAliados) * 3
    scoreOpciones = (opcionesAliados - opcionesRivales) * 20
    
    -- Bonus por estar cerca de ganar
    bonusCercaGanar = if misFichas <= 2 then 100 * (3 - misFichas) else 0
    
  in scoreFichas + scorePuntos + scoreOpciones + bonusCercaGanar

-- | Evaluar estado final (victoria/derrota)
evaluarEstadoFinal :: GameState -> Int -> Int
evaluarEstadoFinal gs miIdx =
  let
    jugadores = gsJugadores gs
    
    -- Buscar si alguien ganó (se quedó sin fichas)
    ganador = filter (\p -> null (playerHand p)) jugadores
    
    (aliados, _) = particionarEquipos miIdx jugadores
    
  in case ganador of
      (g:_) -> if g `elem` aliados
               then 10000   -- Victoria
               else -10000  -- Derrota
      [] ->
        -- Trancado: gana quien tiene menos puntos
        let
          puntosAliados = sum $ map puntosEnMano aliados
          puntosRivales = sum $ map puntosEnMano (filter (`notElem` aliados) jugadores)
        in if puntosAliados < puntosRivales
           then 5000   -- Victoria por puntos
           else if puntosAliados > puntosRivales
             then -5000  -- Derrota por puntos
             else 0      -- Empate

-- | Particionar jugadores en aliados y rivales
particionarEquipos :: Int -> [Player] -> ([Player], [Player])
particionarEquipos miIdx jugadores =
  let
    nJugadores = length jugadores
    indices = [0 .. nJugadores - 1]
    esAliado idx = esDelMismoEquipo miIdx idx nJugadores
    aliadosIdx = filter esAliado indices
    rivalesIdx = filter (not . esAliado) indices
  in (map (jugadores !!) aliadosIdx, map (jugadores !!) rivalesIdx)
