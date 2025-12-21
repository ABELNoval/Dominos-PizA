module Game.Rules
  ( jugadasPosibles
  , puedeJugar
  , estaTrancado
  , determinarGanador
  , ResultadoPartida(..)
  , quienEmpieza
  ) where

import Game.Domino (Domino(..), puedeConectar, esDoble, puntos)
import Game.Board (Board, extremoIzquierdo, extremoDerecho, Lado(..), esTableroVacio)
import Game.Player (Player(..), Hand, puntosEnMano, tieneFichas)
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)

-- | Resultado de una partida.
data ResultadoPartida
  = GanadorDomino Player       -- Un jugador se quedó sin fichas
  | GanadorTrancado Player     -- Partida trancada, gana quien tiene menos puntos
  | Empate [Player]            -- Empate entre varios jugadores
  deriving (Show, Eq)

-- | Devuelve todas las jugadas posibles para un jugador dado el tablero actual.
-- Cada jugada es (Ficha, Lado).
jugadasPosibles :: Hand -> Board -> [(Domino, Lado)]
jugadasPosibles mano board
  | esTableroVacio board = [(ficha, Derecha) | ficha <- mano]  -- Primera jugada: cualquier ficha
  | otherwise            = jugadasIzq ++ jugadasDer
  where
    jugadasIzq = case extremoIzquierdo board of
      Nothing  -> []
      Just ext -> [(ficha, Izquierda) | ficha <- mano, puedeConectar ext ficha]

    jugadasDer = case extremoDerecho board of
      Nothing  -> []
      Just ext -> [(ficha, Derecha) | ficha <- mano, puedeConectar ext ficha]

-- | ¿Puede un jugador hacer alguna jugada?
puedeJugar :: Hand -> Board -> Bool
puedeJugar mano board = not $ null $ jugadasPosibles mano board

-- | ¿El juego está trancado? (ningún jugador puede jugar)
estaTrancado :: [Player] -> Board -> Bool
estaTrancado jugadores board =
  all (\j -> not $ puedeJugar (playerHand j) board) jugadores

-- | Determina quién empieza la partida.
-- Regla clásica: empieza quien tiene el doble más alto.
-- Si nadie tiene dobles, empieza quien tiene la ficha más alta.
quienEmpieza :: [Player] -> Maybe (Player, Domino)
quienEmpieza jugadores =
  case encontrarDobleMasAlto jugadores of
    Just resultado -> Just resultado
    Nothing        -> encontrarFichaMasAlta jugadores

-- | Busca el jugador con el doble más alto.
encontrarDobleMasAlto :: [Player] -> Maybe (Player, Domino)
encontrarDobleMasAlto jugadores =
  let todosDobles = [ (j, ficha)
                    | j <- jugadores
                    , ficha <- playerHand j
                    , esDoble ficha
                    ]
  in if null todosDobles
       then Nothing
       else Just $ maximumBy (comparing (puntos . snd)) todosDobles

-- | Busca el jugador con la ficha de mayor puntuación.
encontrarFichaMasAlta :: [Player] -> Maybe (Player, Domino)
encontrarFichaMasAlta jugadores =
  let todasFichas = [ (j, ficha)
                    | j <- jugadores
                    , ficha <- playerHand j
                    ]
  in if null todasFichas
       then Nothing
       else Just $ maximumBy (comparing (puntos . snd)) todasFichas

-- | Determina el ganador de la partida.
-- Se llama cuando alguien se queda sin fichas o cuando está trancado.
determinarGanador :: [Player] -> Maybe Player -> ResultadoPartida
determinarGanador jugadores (Just ganadorDomino) =
  GanadorDomino ganadorDomino
determinarGanador jugadores Nothing =
  -- Partida trancada: gana quien tiene menos puntos en la mano
  let jugadoresConFichas = filter tieneFichas jugadores
      menorPuntuacion    = minimum $ map puntosEnMano jugadoresConFichas
      ganadores          = filter (\j -> puntosEnMano j == menorPuntuacion) jugadores
  in case ganadores of
       [ganador] -> GanadorTrancado ganador
       varios    -> Empate varios
