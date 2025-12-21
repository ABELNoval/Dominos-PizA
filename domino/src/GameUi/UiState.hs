module GameUi.UiState where

-- | Represents all possible UI screens of the game.
-- | The game UI is always in exactly one of these states.
data UiScreen
    = MainMenu
    | GameModeMenu
    | DifficultyMenu
    | Gameplay
    | Exit
    deriving (Eq, Show)


-- | Represents the complete UI state.
-- | For now, it only contains the current screen.
-- | Later, it will also store selected mode, difficulty, etc.
newtype UiState = UiState
    { currentScreen :: UiScreen
    }
    deriving (Show)


-- | Initial UI state when the game starts.
initialUiState :: UiState
initialUiState = UiState
    { currentScreen = MainMenu
    }
