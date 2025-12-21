module GameUi.UiState where

import Game.GameState (GameState)
import Game.Domino (Domino)
import Game.Board (Lado(..))

data UiScreen
    = MainMenu
    | GameModeMenu
    | DifficultyMenu
    | Gameplay
    | GameOver
    | Exit
    deriving (Eq, Show)

data UiSelection
    = NoSelection
    | Selected Domino
    deriving (Eq, Show)

data UiState = UiState
    { currentScreen :: UiScreen
    , gameState     :: Maybe GameState
    , selection     :: UiSelection
    , message       :: String        -- mensaje de error/info para el jugador
    , winner        :: Maybe String  -- nombre del ganador (si terminó)
    }
    deriving (Show)

initialUiState :: UiState
initialUiState = UiState
    { currentScreen = MainMenu
    , gameState     = Nothing
    , selection     = NoSelection
    , message       = ""
    , winner        = Nothing
    }
