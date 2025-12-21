module Main where

import Graphics.Gloss.Interface.IO.Game


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

main :: IO ()
main =
    playIO
        window
        backgroundColor
        fps
        initialUiState
        (pure . renderUi)
        handleUiEvent
        updateUi



updateUi :: Float -> UiState -> IO UiState
updateUi _ = pure
