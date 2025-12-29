module AI.Hard
  ( chooseHardMove
  ) where

import Game.GameState (GameState(..), jugadorActual, getTablero, getPozo, getJugadores)
import Game.Player (Player(..), playerHand, quitarFicha)
import Game.Rules (jugadasPosibles)
import Game.Actions (Accion(..))
import Game.Domino (Domino(..), esDoble, puntos)
import Game.Board (Board, Lado(..), colocarFicha, extremoIzquierdo, extremoDerecho)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

{- 
  =========================================================
  IA DIFÍCIL - ESTRATEGIA CON PREDICCIÓN DE 1 NIVEL
  =========================================================
  
  La idea es pensar un paso adelante:
  
  1. Para cada jugada mía posible, simulo el estado resultante
  2. Miro qué jugadas tendría el SIGUIENTE jugador (rival)
  3. Evalúo qué tan bueno es ese escenario para mí
  4. Elijo la jugada que me deje en mejor posición
  
  Criterios de evaluación:
  - Menos opciones para el rival = mejor para mí
  - Dejar extremos que yo pueda jugar = mejor para mí
  - Quitar puntos de mi mano = mejor para mí (por si hay trancazo)
-}

-- | Función principal: elegir la mejor jugada
chooseHardMove :: GameState -> Accion
chooseHardMove gs =
  let jugador = jugadorActual gs
      mano = playerHand jugador
      tablero = getTablero gs
      jugadas = jugadasPosibles mano tablero
      pozo = getPozo gs
  in case jugadas of
       [] -> if null pozo then Pasar else Robar
       _  -> 
         -- Evaluar cada jugada con predicción
         let jugadasEvaluadas = map (evaluarJugadaConPrediccion gs) jugadas
             -- Ordenar por puntuación (mayor es mejor)
             mejorJugada = fst $ maximumBy (comparing snd) jugadasEvaluadas
         in uncurry Jugar mejorJugada

-- | Evaluar una jugada simulando la respuesta del rival
-- Devuelve (jugada, puntuación)
evaluarJugadaConPrediccion :: GameState -> (Domino, Lado) -> ((Domino, Lado), Int)
evaluarJugadaConPrediccion gs jugada@(ficha, lado) =
  let 
      -- === PASO 1: Simular mi jugada ===
      tableroActual = getTablero gs
      jugadorActualP = jugadorActual gs
      
      -- Simular colocar la ficha en el tablero
      tableroSimulado = fromMaybe tableroActual (colocarFicha ficha lado tableroActual)
      miManoSimulada = case quitarFicha ficha jugadorActualP of
        Just p  -> playerHand p
        Nothing -> playerHand jugadorActualP
      
      -- === PASO 2: Ver qué puede hacer el rival ===
      jugadores = getJugadores gs
      turnoActual = gsTurnoActual gs
      nJugadores = length jugadores
      turnoRival = (turnoActual + 1) `mod` nJugadores
      rival = jugadores !! turnoRival
      manoRival = playerHand rival
      
      -- Jugadas que el rival tendría después de mi jugada
      jugadasRival = jugadasPosibles manoRival tableroSimulado
      
      -- === PASO 3: Ver qué podría hacer YO después del rival ===
      -- Analizamos los extremos que quedarían después de cada respuesta del rival
      -- y contamos cuántas opciones tendría yo
      
      misOpcionesDespuesDeRival = 
        if null jugadasRival
          then 
            -- Si el rival no puede jugar, los extremos quedan igual
            -- Cuento cuántas fichas de mi mano restante pueden jugar
            length $ jugadasPosibles miManoSimulada tableroSimulado
          else
            -- Para cada posible respuesta del rival, calculo mis opciones
            let opcionesPorRespuesta = map (calcularMisOpciones miManoSimulada) jugadasRival
            in if null opcionesPorRespuesta 
               then 0 
               else minimum opcionesPorRespuesta  -- Asumo que el rival elige lo peor para mí
      
      -- === PASO 4: Calcular puntuación final ===
      
      -- Bonus por reducir opciones del rival (menos opciones = mejor)
      bonusBloqueo = (10 - length jugadasRival) * 20
      
      -- Bonus por mantener mis opciones (más opciones = mejor)
      bonusMisOpciones = misOpcionesDespuesDeRival * 15
      
      -- Bonus por quitar puntos de mi mano (por si hay trancazo)
      bonusPuntos = puntos ficha * 2
      
      -- Bonus por jugar dobles (liberarse de ellos temprano)
      bonusDoble = if esDoble ficha then 25 else 0
      
      -- Bonus si dejo un extremo que tengo en mi mano
      bonusExtremos = calcularBonusExtremos miManoSimulada tableroSimulado
      
      -- Puntuación total
      puntuacionTotal = bonusBloqueo + bonusMisOpciones + bonusPuntos + bonusDoble + bonusExtremos
      
  in (jugada, puntuacionTotal)
  where
    -- Calcular cuántas opciones tendría yo después de una respuesta del rival
    calcularMisOpciones :: [Domino] -> (Domino, Lado) -> Int
    calcularMisOpciones miMano (fichaRival, ladoRival) =
      -- Primero aplico MI jugada al tablero
      let tableroConMiJugada = fromMaybe (getTablero gs) (colocarFicha (fst jugada) (snd jugada) (getTablero gs))
          -- Luego aplico la jugada del rival
          tableroFinal = fromMaybe tableroConMiJugada (colocarFicha fichaRival ladoRival tableroConMiJugada)
      in length $ jugadasPosibles miMano tableroFinal

-- | Calcular bonus por dejar extremos que puedo jugar
calcularBonusExtremos :: [Domino] -> Board -> Int
calcularBonusExtremos mano tablero =
  let extIzq = extremoIzquierdo tablero
      extDer = extremoDerecho tablero
      
      -- Contar cuántas fichas de mi mano pueden jugar en cada extremo
      numerosEnMano = concatMap (\(Domino a b) -> [a, b]) mano
      
      matchIzq = case extIzq of
        Nothing -> 0
        Just e  -> length $ filter (== e) numerosEnMano
      
      matchDer = case extDer of
        Nothing -> 0
        Just e  -> length $ filter (== e) numerosEnMano
        
  in (matchIzq + matchDer) * 10
