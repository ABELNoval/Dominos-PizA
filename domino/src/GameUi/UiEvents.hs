module GameUi.UiEvents where

import Graphics.Gloss.Interface.Pure.Game
import GameUi.UiState
import GameUi.UiButton

-- | Handles all UI-related events.
handleUiEvent :: Event -> UiState -> UiState
handleUiEvent event uiState =
    case event of
        EventKey (MouseButton LeftButton) Up _ mousePos ->
            handleMouseClick mousePos uiState

        _ ->
            uiState


-- | Handles a mouse click depending on the current screen.
handleMouseClick :: (Float, Float) -> UiState -> UiState
handleMouseClick mousePos uiState =
    case clickedButton mousePos (currentScreen uiState) of
        Just button ->
            uiState { currentScreen = nextScreen button }

        Nothing ->
            uiState


-- | Determines if any button was clicked on the current screen.
clickedButton :: (Float, Float) -> UiScreen -> Maybe Button
clickedButton mousePos screen =
    findButton mousePos (buttonsForScreen screen)


-- | Finds the first button that contains the mouse position.
findButton :: (Float, Float) -> [Button] -> Maybe Button
findButton _ [] = Nothing
findButton mousePos (b:bs)
    | isInsideButton mousePos b = Just b
    | otherwise                = findButton mousePos bs
