module Game.GameState
  ( GameState(..)
  , FaseJuego(..)
  , iniciarPartida
  , iniciarPartidaEquipos
  , jugadorActual
  , siguienteTurno
  , actualizarJugador
  , actualizarTablero
  , sacarDelPozo
  , estaTerminado
  , getJugadores
  , getTablero
  , getPozo
  ) where

import Game.Domino (Domino, generarFichasCompletas)
import Game.Board (Board, boardVacio)
import Game.Player (Player(..), Team(..), mkPlayer, mkPlayerWithTeam, agregarFicha)
import Game.Rules (estaTrancado, quienEmpieza)
import System.Random (StdGen, newStdGen)
import System.Random.Shuffle (shuffle')

-- | Fase actual del juego.
data FaseJuego
  = Preparacion       -- Repartiendo fichas
  | EnCurso           -- Partida en progreso
  | Terminado         -- Partida finalizada
  deriving (Show, Eq)

-- | Estado completo del juego.
data GameState = GameState
  { gsJugadores    :: [Player]      -- Lista de jugadores
  , gsTurnoActual  :: Int           -- Índice del jugador que tiene el turno (0-based)
  , gsTablero      :: Board         -- Tablero con las fichas jugadas
  , gsPozo         :: [Domino]      -- Fichas restantes en el pozo
  , gsFase         :: FaseJuego     -- Fase actual del juego
  , gsPasesConsec  :: Int           -- Pases consecutivos (para detectar trancado)
  } deriving (Show)

-- | Crear estado inicial de una partida.
-- Baraja las fichas, reparte a los jugadores, y determina quién empieza.
iniciarPartida :: [String] -> Int -> IO GameState
iniciarPartida nombres fichasPorJugador = do
    gen <- newStdGen
    let todasFichas    = generarFichasCompletas
        fichasBarajadas = shuffle' todasFichas (length todasFichas) gen
        nJugadores     = length nombres
        (manos, pozo)  = repartirFichas fichasBarajadas nJugadores fichasPorJugador
        jugadores      = zipWith3 crearJugador [0..] nombres manos

    -- Determinar quién empieza
    let turnoInicial = case quienEmpieza jugadores of
          Just (jugador, _) -> playerId jugador
          Nothing           -> 0

    pure $ GameState
      { gsJugadores   = jugadores
      , gsTurnoActual = turnoInicial
      , gsTablero     = boardVacio
      , gsPozo        = pozo
      , gsFase        = EnCurso
      , gsPasesConsec = 0
      }
  where
    crearJugador pid nombre mano = (mkPlayer pid nombre) { playerHand = mano }

-- | Crear estado inicial de una partida por equipos (2vs2).
-- Jugadores 0 y 2 son equipo A, jugadores 1 y 3 son equipo B.
-- El orden de turnos es: 0 -> 1 -> 2 -> 3 (alternando equipos).
iniciarPartidaEquipos :: [String] -> Int -> IO GameState
iniciarPartidaEquipos nombres fichasPorJugador = do
    gen <- newStdGen
    let todasFichas     = generarFichasCompletas
        fichasBarajadas = shuffle' todasFichas (length todasFichas) gen
        nJugadores      = length nombres
        (manos, pozo)   = repartirFichas fichasBarajadas nJugadores fichasPorJugador
        -- Asignar equipos: índices 0,2 = TeamA, índices 1,3 = TeamB
        equipos         = [TeamA, TeamB, TeamA, TeamB]
        jugadores       = zipWith4 crearJugadorEquipo [0..] nombres manos equipos

    -- Determinar quién empieza
    let turnoInicial = case quienEmpieza jugadores of
          Just (jugador, _) -> playerId jugador
          Nothing           -> 0

    pure $ GameState
      { gsJugadores   = jugadores
      , gsTurnoActual = turnoInicial
      , gsTablero     = boardVacio
      , gsPozo        = pozo
      , gsFase        = EnCurso
      , gsPasesConsec = 0
      }
  where
    crearJugadorEquipo pid nombre mano team = 
      (mkPlayerWithTeam pid nombre team) { playerHand = mano }

-- | Helper para zipWith con 4 listas
zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 f (a:as) (b:bs) (c:cs) (d:ds) = f a b c d : zipWith4 f as bs cs ds
zipWith4 _ _ _ _ _ = []

-- | Repartir fichas entre jugadores.
repartirFichas :: [Domino] -> Int -> Int -> ([[Domino]], [Domino])
repartirFichas fichas nJugadores fichasPorJugador =
    let totalRepartir = nJugadores * fichasPorJugador
        (paraRepartir, pozo) = splitAt totalRepartir fichas
        manos = chunksOf fichasPorJugador paraRepartir
    in (manos, pozo)

-- | Dividir lista en trozos.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Obtener el jugador que tiene el turno actual.
jugadorActual :: GameState -> Player
jugadorActual gs = gsJugadores gs !! gsTurnoActual gs

-- | Pasar al siguiente turno.
siguienteTurno :: GameState -> GameState
siguienteTurno gs =
    let nJugadores   = length (gsJugadores gs)
        nuevoTurno   = (gsTurnoActual gs + 1) `mod` nJugadores
    in gs { gsTurnoActual = nuevoTurno }

-- | Actualizar un jugador en el estado.
actualizarJugador :: Player -> GameState -> GameState
actualizarJugador jugador gs =
    gs { gsJugadores = map actualizarSiCoincide (gsJugadores gs) }
  where
    actualizarSiCoincide j
      | playerId j == playerId jugador = jugador
      | otherwise                       = j

-- | Actualizar el tablero.
actualizarTablero :: Board -> GameState -> GameState
actualizarTablero board gs = gs { gsTablero = board }

-- | Sacar una ficha del pozo (si hay).
-- Devuelve (Maybe ficha sacada, nuevo estado).
sacarDelPozo :: GameState -> (Maybe Domino, GameState)
sacarDelPozo gs =
    case gsPozo gs of
      []     -> (Nothing, gs)
      (f:fs) -> (Just f, gs { gsPozo = fs })

-- | ¿La partida ha terminado?
estaTerminado :: GameState -> Bool
estaTerminado gs = gsFase gs == Terminado

-- | Getters convenientes.
getJugadores :: GameState -> [Player]
getJugadores = gsJugadores

getTablero :: GameState -> Board
getTablero = gsTablero

getPozo :: GameState -> [Domino]
getPozo = gsPozo
