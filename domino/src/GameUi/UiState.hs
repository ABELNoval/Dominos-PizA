module GameUi.UiState where

import Game.GameState (GameState)
import Game.Domino (Domino)

data UiScreen
    = MainMenu
    | GameModeMenu
    | DifficultyMenu
    | Gameplay
    | Exit
    deriving (Eq, Show)

data UiSelection
    = NoSelection
    | Selected Domino
    deriving (Eq, Show)

data UiState = UiState
    { currentScreen :: UiScreen
    , gameState     :: Maybe GameState
    , selection :: UiSelection
    }
    deriving (Show)

initialUiState :: UiState
initialUiState = UiState
    { currentScreen = MainMenu
    , gameState     = Nothing
    , selection = NoSelection
    }
