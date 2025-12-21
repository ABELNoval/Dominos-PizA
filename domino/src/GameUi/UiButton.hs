module GameUi.UiButton where

import GameUi.UiState (UiScreen(..))

-- | Represents a rectangular clickable button.
-- | The action is modeled as a screen transition.
data Button = Button
    { buttonX      :: Float   -- ^ Center X position
    , buttonY      :: Float   -- ^ Center Y position
    , buttonWidth  :: Float   -- ^ Width of the rectangle
    , buttonHeight :: Float   -- ^ Height of the rectangle
    , nextScreen   :: UiScreen -- ^ Screen to switch to when clicked
    }
    deriving (Show)


-- | Checks whether a point (mouse click) is inside a button rectangle.
isInsideButton :: (Float, Float) -> Button -> Bool
isInsideButton (mx, my) button =
    mx >= left   &&
    mx <= right  &&
    my >= bottom &&
    my <= top
  where
    halfW = buttonWidth button / 2
    halfH = buttonHeight button / 2

    left   = buttonX button - halfW
    right  = buttonX button + halfW
    bottom = buttonY button - halfH
    top    = buttonY button + halfH


-- =========================================================
-- Buttons for each UI screen
-- =========================================================

-- | Buttons shown in the Main Menu.
mainMenuButtons :: [Button]
mainMenuButtons =
    [ Button 0   50  200 60 GameModeMenu  -- Play
    , Button 0 (-50) 200 60 Exit          -- Exit
    ]


-- | Buttons shown in the Game Mode selection menu.
gameModeMenuButtons :: [Button]
gameModeMenuButtons =
    [ Button 0   50  250 60 DifficultyMenu -- Mode selection (placeholder)
    ]


-- | Buttons shown in the Difficulty selection menu.
difficultyMenuButtons :: [Button]
difficultyMenuButtons =
    [ Button 0   80  200 60 Gameplay  -- Easy
    , Button 0    0  200 60 Gameplay  -- Medium
    , Button 0 (-80) 200 60 Gameplay  -- Hard
    ]


-- | Returns the buttons corresponding to the current UI screen.
buttonsForScreen :: UiScreen -> [Button]
buttonsForScreen screen =
    case screen of
        MainMenu        -> mainMenuButtons
        GameModeMenu    -> gameModeMenuButtons
        DifficultyMenu  -> difficultyMenuButtons
        _               -> []
