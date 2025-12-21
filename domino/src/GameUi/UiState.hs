module GameUi.UiState where

import Game.GameState (GameState)

data UiScreen
    = MainMenu
    | GameModeMenu
    | DifficultyMenu
    | Gameplay
    | Exit
    deriving (Eq, Show)

data UiState = UiState
    { currentScreen :: UiScreen
    , gameState     :: Maybe GameState
    }
    deriving (Show)

initialUiState :: UiState
initialUiState = UiState
    { currentScreen = MainMenu
    , gameState     = Nothing
    }
