module Game.Player
  ( Player(..)
  , Hand
  , Team(..)
  , mkPlayer
  , mkPlayerWithTeam
  , tieneFichas
  , cantidadFichas
  , quitarFicha
  , agregarFicha
  , puntosEnMano
  , tieneFicha
  , esDelEquipo
  , companerosDeEquipo
  ) where

import Game.Domino (Domino, puntos)

-- | Mano de un jugador: lista de fichas.
type Hand = [Domino]

-- | Equipo del jugador (para modo 2vs2)
data Team = TeamA | TeamB | NoTeam
  deriving (Show, Eq)

-- | Un jugador con identificador, nombre, mano de fichas y equipo.
data Player = Player
  { playerId   :: Int
  , playerName :: String
  , playerHand :: Hand
  , playerTeam :: Team
  } deriving (Show, Eq)

-- | Crear un jugador con mano vacía (sin equipo).
mkPlayer :: Int -> String -> Player
mkPlayer pid name = Player
  { playerId   = pid
  , playerName = name
  , playerHand = []
  , playerTeam = NoTeam
  }

-- | Crear un jugador con mano vacía y equipo asignado.
mkPlayerWithTeam :: Int -> String -> Team -> Player
mkPlayerWithTeam pid name team = Player
  { playerId   = pid
  , playerName = name
  , playerHand = []
  , playerTeam = team
  }

-- | ¿El jugador tiene fichas en la mano?
tieneFichas :: Player -> Bool
tieneFichas = not . null . playerHand

-- | Cantidad de fichas en la mano.
cantidadFichas :: Player -> Int
cantidadFichas = length . playerHand

-- | Quitar una ficha de la mano del jugador.
-- Devuelve Nothing si el jugador no tiene esa ficha.
quitarFicha :: Domino -> Player -> Maybe Player
quitarFicha ficha player =
  if ficha `elem` playerHand player
    then Just player { playerHand = remove ficha (playerHand player) }
    else Nothing
  where
    remove _ []     = []
    remove x (y:ys)
      | x == y    = ys
      | otherwise = y : remove x ys

-- | Agregar una ficha a la mano del jugador (al robar del pozo).
agregarFicha :: Domino -> Player -> Player
agregarFicha ficha player =
  player { playerHand = ficha : playerHand player }

-- | Total de puntos en la mano del jugador.
puntosEnMano :: Player -> Int
puntosEnMano = sum . map puntos . playerHand

-- | ¿El jugador tiene esta ficha en la mano?
tieneFicha :: Domino -> Player -> Bool
tieneFicha ficha player = ficha `elem` playerHand player

-- | ¿El jugador pertenece a un equipo específico?
esDelEquipo :: Team -> Player -> Bool
esDelEquipo team player = playerTeam player == team

-- | Obtener compañeros de equipo (incluido el jugador mismo)
companerosDeEquipo :: Player -> [Player] -> [Player]
companerosDeEquipo player players = 
  case playerTeam player of
    NoTeam -> [player]
    team   -> filter (esDelEquipo team) players
