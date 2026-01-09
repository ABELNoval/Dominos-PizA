module AI.Hard2v2
  ( chooseHard2v2Move
  , PlayedHistory
  ) where

import Game.GameState
import Game.Player
import Game.Rules
import Game.Actions
import Game.Domino
import Game.Board

import Data.List (sortBy, maximumBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- CONFIGURACIÓN GENERAL
--------------------------------------------------------------------------------

maxNumber :: Int
maxNumber = 9

initialOccurrencesPerNumber :: Int
initialOccurrencesPerNumber = maxNumber + 2

topK :: Int
topK = 3

--------------------------------------------------------------------------------
-- MODOS DE JUEGO
--------------------------------------------------------------------------------

data Mode = Atacante | Defensivo
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- MEMORIA GLOBAL
--------------------------------------------------------------------------------

-- | Historial de fichas jugadas: (Domino, índice del jugador que la jugó)
type PlayedHistory = [(Domino, Int)]

data NumberStats = NumberStats
  { totalRestantes     :: Int
  , jugadasPorEquipo   :: Int
  , jugadasPorRival    :: Int
  }

type NumberStatsMap = Map.Map Int NumberStats

--------------------------------------------------------------------------------
-- ENTRADA PRINCIPAL
--------------------------------------------------------------------------------

-- | Elegir jugada para el modo difícil en 2vs2
-- Recibe el historial de jugadas como parámetro adicional
chooseHard2v2Move :: PlayedHistory -> GameState -> Accion
chooseHard2v2Move history gs =
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
          modo = decidirModo jugadorIdx jugadores

          jugadasOrdenadas =
            ordenarPorHeuristico modo jugadorIdx history mano tablero jugadas

          mejores = take topK jugadasOrdenadas

          evaluadas =
            map (evaluarJugada2v2 gs modo history) mejores

          mejor = fst $ maximumBy (comparing snd) evaluadas
        in uncurry Jugar mejor

--------------------------------------------------------------------------------
-- DECISIÓN AUTOMÁTICA DE MODO
--------------------------------------------------------------------------------

decidirModo :: Int -> [Player] -> Mode
decidirModo myIdx jugadores =
  let
    nJugadores = length jugadores
    compIdx = (myIdx + 2) `mod` nJugadores
    miMano  = length . playerHand $ jugadores !! myIdx
    coMano  = if nJugadores > 2 
              then length . playerHand $ jugadores !! compIdx
              else miMano
  in if miMano >= coMano then Defensivo else Atacante

--------------------------------------------------------------------------------
-- ORDENAMIENTO HEURÍSTICO
--------------------------------------------------------------------------------

ordenarPorHeuristico
  :: Mode -> Int -> PlayedHistory -> [Domino] -> Board -> [(Domino,Lado)]
  -> [(Domino,Lado)]
ordenarPorHeuristico mode myIdx history mano tablero =
  sortBy (flip (comparing (heuristicoJugada mode myIdx history mano tablero)))

heuristicoJugada
  :: Mode -> Int -> PlayedHistory -> [Domino] -> Board -> (Domino,Lado) -> Int
heuristicoJugada mode myIdx history mano tablero (ficha,lado) =
  let
    tablero' = fromMaybe tablero (colocarFicha ficha lado tablero)
    stats    = buildStats myIdx history

    scorePuntos = puntos ficha * 5
    scoreDoble  = if esDoble ficha then 25 else 0

    bonusEquipo = calcularBonusExtremos mano tablero'
    penalExt    = penalizarExtremos mode stats tablero tablero'
    bonusSenal  = bonusSenalEquipo mode stats tablero tablero'
    penalRompe  = penalRomperSenalEquipo mode stats tablero tablero'

  in scorePuntos + scoreDoble + bonusEquipo + bonusSenal - penalExt - penalRompe

--------------------------------------------------------------------------------
-- EVALUACIÓN CON PREDICCIÓN (MINIMAX REDUCIDO)
--------------------------------------------------------------------------------

evaluarJugada2v2
  :: GameState -> Mode -> PlayedHistory -> (Domino,Lado) -> ((Domino,Lado), Int)
evaluarJugada2v2 gs _ _ jugada@(ficha,lado) =
  let
    jugadores = getJugadores gs
    n = length jugadores
    idx = gsTurnoActual gs

    idxR1 = (idx+1) `mod` n
    idxC  = (idx+2) `mod` n
    idxR2 = (idx+3) `mod` n

    tablero0 = getTablero gs
    tablero1 = fromMaybe tablero0 (colocarFicha ficha lado tablero0)

    manoC = if n > 2 then playerHand (jugadores !! idxC) else []
    manoR1 = playerHand (jugadores !! idxR1)
    manoR2 = if n > 3 then playerHand (jugadores !! idxR2) else []

    score =
      let
        opp1 = jugadasPosibles manoR1 tablero1
      in if null opp1
         then evaluarEstado tablero1 manoC manoR1 manoR2
         else minimum $
              map (\(f,l) ->
                    let t2 = fromMaybe tablero1 (colocarFicha f l tablero1)
                    in evaluarEstado t2 manoC manoR1 manoR2
                  ) (take topK opp1)
  in (jugada, score)

evaluarEstado :: Board -> [Domino] -> [Domino] -> [Domino] -> Int
evaluarEstado tablero manoC manoR1 manoR2 =
  let
    teamOpts = length (jugadasPosibles manoC tablero)
    oppOpts  = length (jugadasPosibles manoR1 tablero)
             + length (jugadasPosibles manoR2 tablero)
  in teamOpts * 20 - oppOpts * 30

--------------------------------------------------------------------------------
-- MEMORIA Y SEÑALES
--------------------------------------------------------------------------------

buildStats :: Int -> PlayedHistory -> NumberStatsMap
buildStats myIdx history =
  let
    base = Map.fromList
      [ (n, NumberStats initialOccurrencesPerNumber 0 0)
      | n <- [0..maxNumber]
      ]

    esEquipo i = i `mod` 2 == myIdx `mod` 2

    aplicar m (Domino a b, idx) =
      let upd n =
            Map.adjust
              (\s -> s
                { totalRestantes = totalRestantes s - 1
                , jugadasPorEquipo = jugadasPorEquipo s + if esEquipo idx then 1 else 0
                , jugadasPorRival  = jugadasPorRival  s + if esEquipo idx then 0 else 1
                })
              n
      in if a == b then upd a m else upd b (upd a m)
  in foldl aplicar base history

penalizarExtremos :: Mode -> NumberStatsMap -> Board -> Board -> Int
penalizarExtremos mode stats b0 b1 =
  let
    factor = if mode == Atacante then 0.5 else 1.5
    extremos b = (extremoIzquierdo b, extremoDerecho b)

    penal (Just n) =
      case Map.lookup n stats of
        Just s ->
          let base' = totalRestantes s * 15
              extra = if jugadasPorRival s > jugadasPorEquipo s then 25 else 0
          in round (fromIntegral (base' + extra) * factor)
        _ -> 0
    penal _ = 0

    (i0,d0) = extremos b0
    (i1,d1) = extremos b1
  in (if i0 /= i1 then penal i1 else 0)
   + (if d0 /= d1 then penal d1 else 0)

bonusSenalEquipo :: Mode -> NumberStatsMap -> Board -> Board -> Int
bonusSenalEquipo mode stats b0 b1 =
  let
    bonus n =
      case Map.lookup n stats of
        Just s | jugadasPorEquipo s > jugadasPorRival s ->
          if mode == Defensivo then 30 else 10
        _ -> 0
    extremos b = (extremoIzquierdo b, extremoDerecho b)
    (i0,d0) = extremos b0
    (i1,d1) = extremos b1
  in (if i0 /= i1 then maybe 0 bonus i1 else 0)
   + (if d0 /= d1 then maybe 0 bonus d1 else 0)

penalRomperSenalEquipo :: Mode -> NumberStatsMap -> Board -> Board -> Int
penalRomperSenalEquipo Defensivo stats b0 b1 =
  let
    rompe (Just n) =
      case Map.lookup n stats of
        Just s -> jugadasPorEquipo s > jugadasPorRival s
        _ -> False
    rompe _ = False
    extremos b = (extremoIzquierdo b, extremoDerecho b)
    (i0,d0) = extremos b0
    (i1,d1) = extremos b1
  in (if rompe i0 && i0 /= i1 then 25 else 0)
   + (if rompe d0 && d0 /= d1 then 25 else 0)
penalRomperSenalEquipo _ _ _ _ = 0

--------------------------------------------------------------------------------
-- BONUS POR EXTREMOS
--------------------------------------------------------------------------------

calcularBonusExtremos :: [Domino] -> Board -> Int
calcularBonusExtremos mano tablero =
  let extIzq = extremoIzquierdo tablero
      extDer = extremoDerecho tablero
      
      numerosEnMano = concatMap (\(Domino a b) -> [a, b]) mano
      
      matchIzq = case extIzq of
        Nothing -> 0
        Just e  -> length $ filter (== e) numerosEnMano
      
      matchDer = case extDer of
        Nothing -> 0
        Just e  -> length $ filter (== e) numerosEnMano
        
  in (matchIzq + matchDer) * 10
