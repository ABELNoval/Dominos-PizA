module Main where

import Graphics.Gloss.Interface.Pure.Game

import GameUi.UiState
import GameUi.UiRender
import GameUi.UiEvents

-- | Window configuration
window :: Display
window = InWindow
    "Domino Game"
    (800, 600)
    (100, 100)

-- | Background color
backgroundColor :: Color
backgroundColor = black

-- | Frames per second
fps :: Int
fps = 60

-- | Entry point of the application
main :: IO ()
main =
    play
        window
        backgroundColor
        fps
        initialUiState
        renderUi
        handleUiEvent
        updateUi


-- | UI update function.
-- | For now, the UI does not need time-based updates.
updateUi :: Float -> UiState -> UiState
updateUi _ uiState = uiState
