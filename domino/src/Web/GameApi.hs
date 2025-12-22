{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.GameApi 
    ( GameSession(..)
    , GameAction(..)
    , GameResponse(..)
    , GameConfig(..)
    , GameMode(..)
    , VsMode(..)
    , Difficulty(..)
    , DominoJson(..)
    , PlayerJson(..)
    , BoardJson(..)
    , createNewGame
    , executeGameAction
    , gameStateToJson
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), (.:?), (.!=), withObject)
import qualified Data.Text as T

import Game.Domino (Domino(..))
import Game.Board (Board, Lado(..), fichasEnMesa, extremoIzquierdo, extremoDerecho)
import Game.Player (Player(..), playerHand)
import Game.GameState (GameState(..), FaseJuego(..), iniciarPartida, jugadorActual, estaTerminado, getTablero, getJugadores, getPozo, siguienteTurno)
import Game.Actions (Accion(..), ResultadoAccion(..), ejecutarAccion)
import Game.Rules (ResultadoPartida(..), puedeJugar, determinarGanador)

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
    , grGameMode :: T.Text
    , grVsMode :: T.Text
    , grDifficulty :: T.Text
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
-- Game Configuration
-- =========================================================

-- | Modo de juego: si se puede robar o no
data GameMode = Robadito | NoRobadito
    deriving (Show, Eq, Generic)

instance ToJSON GameMode
instance FromJSON GameMode where
    parseJSON = withObject "GameMode" $ \v -> do
        mode <- v .: "gameMode"
        case (mode :: T.Text) of
            "robadito" -> return Robadito
            "norobadito" -> return NoRobadito
            _ -> return Robadito

-- | Modo VS: cuántos jugadores
data VsMode = OneVsOne | FreeForAll | TwoVsTwo
    deriving (Show, Eq, Generic)

instance ToJSON VsMode
instance FromJSON VsMode where
    parseJSON = withObject "VsMode" $ \v -> do
        mode <- v .: "vsMode"
        case (mode :: T.Text) of
            "1vs1" -> return OneVsOne
            "freeforall" -> return FreeForAll
            "2vs2" -> return TwoVsTwo
            _ -> return FreeForAll

-- | Dificultad de la IA
data Difficulty = Easy | Medium | Hard
    deriving (Show, Eq, Generic)

instance ToJSON Difficulty
instance FromJSON Difficulty where
    parseJSON = withObject "Difficulty" $ \v -> do
        diff <- v .: "difficulty"
        case (diff :: T.Text) of
            "easy" -> return Easy
            "medium" -> return Medium
            "hard" -> return Hard
            _ -> return Easy

-- | Configuración completa del juego
data GameConfig = GameConfig
    { gcGameMode :: GameMode
    , gcVsMode :: VsMode
    , gcDifficulty :: Difficulty
    } deriving (Show, Eq, Generic)

instance FromJSON GameConfig where
    parseJSON = withObject "GameConfig" $ \v -> do
        gm <- v .:? "gameMode" .!= "robadito"
        vs <- v .:? "vsMode" .!= "freeforall"
        diff <- v .:? "difficulty" .!= "easy"
        let gameMode = case (gm :: T.Text) of
              "robadito" -> Robadito
              "norobadito" -> NoRobadito
              _ -> Robadito
        let vsMode = case (vs :: T.Text) of
              "1vs1" -> OneVsOne
              "freeforall" -> FreeForAll
              "2vs2" -> TwoVsTwo
              _ -> FreeForAll
        let difficulty = case (diff :: T.Text) of
              "easy" -> Easy
              "medium" -> Medium
              "hard" -> Hard
              _ -> Easy
        return $ GameConfig gameMode vsMode difficulty

-- =========================================================
-- Game Session
-- =========================================================

data GameSession = GameSession
    { gsState :: GameState
    , gsSessionMessage :: T.Text
    , gsGameMode :: GameMode
    , gsVsMode :: VsMode
    , gsDifficulty :: Difficulty
    , gsWinner :: Maybe T.Text
    }

-- =========================================================
-- API Functions
-- =========================================================

-- | Crear un nuevo juego
createNewGame :: GameConfig -> IO GameSession
createNewGame config = do
    let nombres = case gcVsMode config of
            OneVsOne -> ["Tú", "Bot 1"]
            FreeForAll -> ["Tú", "Bot 1", "Bot 2", "Bot 3"]
            TwoVsTwo -> ["Tú", "Compañero", "Rival 1", "Rival 2"]
        fichasPorJugador = case gcVsMode config of
            OneVsOne -> 10
            FreeForAll -> 10
            TwoVsTwo -> 10
    estado <- iniciarPartida nombres fichasPorJugador
    let jugador = jugadorActual estado
        diffText = case gcDifficulty config of
            Easy -> "Fácil"
            Medium -> "Medio"
            Hard -> "Difícil"
        modeText = case gcGameMode config of
            Robadito -> "Robadito"
            NoRobadito -> "Sin robar"
        vsText = case gcVsMode config of
            OneVsOne -> "1 vs 1"
            FreeForAll -> "Todos vs Todos"
            TwoVsTwo -> "2 vs 2"
    return $ GameSession 
        estado 
        (T.pack $ "¡Juego iniciado! " ++ modeText ++ " | " ++ vsText ++ " | Dificultad: " ++ diffText ++ ". Turno de " ++ playerName jugador)
        (gcGameMode config)
        (gcVsMode config)
        (gcDifficulty config)
        Nothing

-- | Ejecutar una acción del jugador
executeGameAction :: GameSession -> GameAction -> GameSession
executeGameAction session action =
    let estado = gsState session
        gameMode = gsGameMode session
        accion = parseAction action gameMode
    in case accion of
        Nothing -> session { gsSessionMessage = "Acción inválida" }
        Just acc -> 
            -- En modo NoRobadito, usamos una validación especial para Pasar
            case (acc, gameMode) of
                (Pasar, NoRobadito) -> ejecutarPasarNoRobadito session estado
                _ -> case ejecutarAccion acc estado of
                    Exito nuevoEstado -> 
                        let msg = T.pack $ "Turno de " ++ playerName (jugadorActual nuevoEstado)
                        in session { gsState = nuevoEstado, gsSessionMessage = msg, gsWinner = Nothing }
                    Victoria ganador nuevoEstado ->
                        let msg = T.pack $ "¡Victoria! " ++ playerName ganador ++ " ganó"
                            winner = T.pack $ playerName ganador
                        in session { gsState = nuevoEstado, gsSessionMessage = msg, gsWinner = Just winner }
                    Trancado resultado nuevoEstado ->
                        let (msg, winner) = case resultado of
                                GanadorDomino p -> 
                                    ("Trancado. Gana " ++ playerName p, Just $ T.pack $ playerName p)
                                GanadorTrancado p -> 
                                    ("Trancado. Gana " ++ playerName p ++ " (menos puntos)", Just $ T.pack $ playerName p)
                                Empate _ -> 
                                    ("Trancado. ¡Empate!", Nothing)
                        in session { gsState = nuevoEstado, gsSessionMessage = T.pack msg, gsWinner = winner }
                    ErrorAccion err -> 
                        session { gsSessionMessage = T.pack err }

-- | Ejecutar pasar en modo NoRobadito (permite pasar aunque haya pozo)
ejecutarPasarNoRobadito :: GameSession -> GameState -> GameSession
ejecutarPasarNoRobadito session gs =
    let jugador = jugadorActual gs
        tablero = getTablero gs
    in if puedeJugar (playerHand jugador) tablero
         then session { gsSessionMessage = "No puedes pasar si tienes jugadas disponibles" }
         else
             let nuevosPases = gsPasesConsec gs + 1
                 nJugadores  = length (getJugadores gs)
                 gs1 = gs { gsPasesConsec = nuevosPases }
             in if nuevosPases >= nJugadores
                  then -- Todos pasaron, partida trancada
                    let resultado = determinarGanador (getJugadores gs) Nothing
                        gsFinal   = gs1 { gsFase = Terminado }
                        (msg, winner) = case resultado of
                            GanadorDomino p -> 
                                ("Trancado. Gana " ++ playerName p, Just $ T.pack $ playerName p)
                            GanadorTrancado p -> 
                                ("Trancado. Gana " ++ playerName p ++ " (menos puntos)", Just $ T.pack $ playerName p)
                            Empate _ -> 
                                ("Trancado. ¡Empate!", Nothing)
                    in session { gsState = gsFinal, gsSessionMessage = T.pack msg, gsWinner = winner }
                  else 
                    let nuevoEstado = siguienteTurno gs1
                        msg = T.pack $ "Turno de " ++ playerName (jugadorActual nuevoEstado)
                    in session { gsState = nuevoEstado, gsSessionMessage = msg, gsWinner = Nothing }

-- | Parsear acción del JSON (considera el modo de juego)
parseAction :: GameAction -> GameMode -> Maybe Accion
parseAction (GameAction "play" (Just dj) (Just side)) _ =
    let domino = Domino (djLeft dj) (djRight dj)
        lado = if side == "left" then Izquierda else Derecha
    in Just $ Jugar domino lado
parseAction (GameAction "pass" _ _) _ = Just Pasar
parseAction (GameAction "draw" _ _) NoRobadito = Just Pasar  -- En NoRobadito, draw se convierte en pass
parseAction (GameAction "draw" _ _) Robadito = Just Robar
parseAction _ _ = Nothing

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
        canDrawBasedOnMode = case gsGameMode session of
            Robadito -> not (null pozo) && not terminado
            NoRobadito -> False
        gameModeText = case gsGameMode session of
            Robadito -> "robadito"
            NoRobadito -> "norobadito"
        vsModeText = case gsVsMode session of
            OneVsOne -> "1vs1"
            FreeForAll -> "freeforall"
            TwoVsTwo -> "2vs2"
        difficultyText = case gsDifficulty session of
            Easy -> "easy"
            Medium -> "medium"
            Hard -> "hard"
    in GameResponse
        { grBoard = boardToJson tablero
        , grPlayers = map playerToJson jugadores
        , grCurrentPlayer = turnoActual
        , grCurrentPlayerName = T.pack $ playerName jugadorAct
        , grMessage = gsSessionMessage session
        , grGameOver = terminado
        , grWinner = gsWinner session
        , grPozoCount = length pozo
        , grCanDraw = canDrawBasedOnMode
        , grGameMode = gameModeText
        , grVsMode = vsModeText
        , grDifficulty = difficultyText
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
