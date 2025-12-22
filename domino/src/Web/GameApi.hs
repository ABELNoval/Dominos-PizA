{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.GameApi 
    ( GameSession(..)
    , GameAction(..)
    , GameResponse(..)
    , DominoJson(..)
    , PlayerJson(..)
    , BoardJson(..)
    , Difficulty(..)
    , createNewGame
    , executeGameAction
    , gameStateToJson
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), withObject)
import qualified Data.Text as T

import Game.Domino (Domino(..))
import Game.Board (Board, Lado(..), fichasEnMesa, extremoIzquierdo, extremoDerecho)
import Game.Player (Player(..))
import Game.GameState (GameState(..), iniciarPartida, jugadorActual, estaTerminado, getTablero, getJugadores, getPozo)
import Game.Actions (Accion(..), ResultadoAccion(..), ejecutarAccion)
import Game.Rules (ResultadoPartida(..))

-- =========================================================
-- JSON Data Types
-- =========================================================

data DominoJson = DominoJson
    { djLeft  :: Int
    , djRight :: Int
    } deriving (Show, Generic)

instance ToJSON DominoJson where
    toJSON (DominoJson l r) = object ["left" .= l, "right" .= r]

instance FromJSON DominoJson where
    parseJSON = withObject "DominoJson" $ \v ->
        DominoJson <$> v .: "left" <*> v .: "right"

data PlayerJson = PlayerJson
    { pjName :: T.Text
    , pjHand :: [DominoJson]
    , pjHandCount :: Int
    } deriving (Show, Generic)

instance ToJSON PlayerJson

data BoardJson = BoardJson
    { bjTiles :: [DominoJson]
    , bjLeftEnd :: Maybe Int
    , bjRightEnd :: Maybe Int
    } deriving (Show, Generic)

instance ToJSON BoardJson

data GameResponse = GameResponse
    { grBoard :: BoardJson
    , grPlayers :: [PlayerJson]
    , grCurrentPlayer :: Int
    , grCurrentPlayerName :: T.Text
    , grMessage :: T.Text
    , grGameOver :: Bool
    , grWinner :: Maybe T.Text
    , grPozoCount :: Int
    , grCanDraw :: Bool
    } deriving (Show, Generic)

instance ToJSON GameResponse

data GameAction = GameAction
    { gaAction :: T.Text  -- "play", "pass", "draw"
    , gaDomino :: Maybe DominoJson
    , gaSide :: Maybe T.Text  -- "left", "right"
    } deriving (Show, Generic)

instance FromJSON GameAction where
    parseJSON = withObject "GameAction" $ \v ->
        GameAction <$> v .: "action" 
                   <*> v .: "domino"
                   <*> v .: "side"

-- =========================================================
-- Game Session
-- =========================================================

data Difficulty = Facil | Medio | Dificil
    deriving (Show, Eq, Generic)

instance FromJSON Difficulty where
    parseJSON = withObject "Difficulty" $ \v -> do
        diff <- v .: "difficulty"
        case (diff :: T.Text) of
            "facil" -> return Facil
            "medio" -> return Medio
            "dificil" -> return Dificil
            _ -> return Facil  -- Default

data GameSession = GameSession
    { gsState :: GameState
    , gsSessionMessage :: T.Text
    , gsDifficulty :: Difficulty
    , gsWinner :: Maybe T.Text
    }

-- =========================================================
-- API Functions
-- =========================================================

-- | Crear un nuevo juego
createNewGame :: Difficulty -> IO GameSession
createNewGame difficulty = do
    let nombres = ["Tú", "Bot 1", "Bot 2", "Bot 3"]
        fichasPorJugador = 10  -- Dominó doble-nueve: 10 fichas por jugador
    estado <- iniciarPartida nombres fichasPorJugador
    let jugador = jugadorActual estado
        diffText = case difficulty of
            Facil -> "Fácil"
            Medio -> "Medio"
            Dificil -> "Difícil"
    return $ GameSession estado (T.pack $ "¡Juego iniciado! Dificultad: " ++ diffText ++ ". Turno de " ++ playerName jugador) difficulty Nothing

-- | Ejecutar una acción del jugador
executeGameAction :: GameSession -> GameAction -> GameSession
executeGameAction session action =
    let estado = gsState session
        accion = parseAction action
    in case accion of
        Nothing -> session { gsSessionMessage = "Acción inválida" }
        Just acc -> 
            case ejecutarAccion acc estado of
                Exito nuevoEstado -> 
                    let msg = T.pack $ "Turno de " ++ playerName (jugadorActual nuevoEstado)
                    in GameSession nuevoEstado msg (gsDifficulty session) Nothing
                Victoria ganador nuevoEstado ->
                    let msg = T.pack $ "¡Victoria! " ++ playerName ganador ++ " ganó"
                        winner = T.pack $ playerName ganador
                    in GameSession nuevoEstado msg (gsDifficulty session) (Just winner)
                Trancado resultado nuevoEstado ->
                    let (msg, winner) = case resultado of
                            GanadorDomino p -> 
                                ("Trancado. Gana " ++ playerName p, Just $ T.pack $ playerName p)
                            GanadorTrancado p -> 
                                ("Trancado. Gana " ++ playerName p ++ " (menos puntos)", Just $ T.pack $ playerName p)
                            Empate _ -> 
                                ("Trancado. ¡Empate!", Nothing)
                    in GameSession nuevoEstado (T.pack msg) (gsDifficulty session) winner
                ErrorAccion err -> 
                    session { gsSessionMessage = T.pack err }

-- | Parsear acción del JSON
parseAction :: GameAction -> Maybe Accion
parseAction (GameAction "play" (Just dj) (Just side)) =
    let domino = Domino (djLeft dj) (djRight dj)
        lado = if side == "left" then Izquierda else Derecha
    in Just $ Jugar domino lado
parseAction (GameAction "pass" _ _) = Just Pasar
parseAction (GameAction "draw" _ _) = Just Robar
parseAction _ = Nothing

-- | Convertir estado del juego a JSON
gameStateToJson :: GameSession -> GameResponse
gameStateToJson session =
    let estado = gsState session
        tablero = getTablero estado
        jugadores = getJugadores estado
        turnoActual = gsTurnoActual estado
        jugadorAct = jugadores !! turnoActual
        terminado = estaTerminado estado
        pozo = getPozo estado
    in GameResponse
        { grBoard = boardToJson tablero
        , grPlayers = map playerToJson jugadores
        , grCurrentPlayer = turnoActual
        , grCurrentPlayerName = T.pack $ playerName jugadorAct
        , grMessage = gsSessionMessage session
        , grGameOver = terminado
        , grWinner = gsWinner session
        , grPozoCount = length pozo
        , grCanDraw = not (null pozo) && not terminado
        }

boardToJson :: Board -> BoardJson
boardToJson board = BoardJson
    { bjTiles = map dominoToJson (fichasEnMesa board)
    , bjLeftEnd = extremoIzquierdo board
    , bjRightEnd = extremoDerecho board
    }

playerToJson :: Player -> PlayerJson
playerToJson player = PlayerJson
    { pjName = T.pack $ playerName player
    , pjHand = map dominoToJson (playerHand player)
    , pjHandCount = length (playerHand player)
    }

dominoToJson :: Domino -> DominoJson
dominoToJson (Domino a b) = DominoJson a b
