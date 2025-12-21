module GameUi.UiEvents where

import Graphics.Gloss.Interface.IO.Game
import GameUi.UiState
import GameUi.UiButton
import Game.GameState (iniciarPartida)

handleUiEvent :: Event -> UiState -> IO UiState
handleUiEvent event uiState =
    case event of
        EventKey (MouseButton LeftButton) Up _ mousePos ->
            handleMouseClick mousePos uiState
        _ ->
            pure uiState


handleMouseClick :: (Float, Float) -> UiState -> IO UiState
handleMouseClick mousePos uiState =
    case getClickedButton mousePos (currentScreen uiState) of
        Just button ->
            handleButtonAction button uiState
        Nothing ->
            pure uiState


handleButtonAction :: Button -> UiState -> IO UiState
handleButtonAction button uiState =
    case nextScreen button of
        Gameplay -> do
            -- AQUÍ SE CREA EL GAMESTATE REAL
            gs <- iniciarPartida ["Player 1", "Player 2"] 7
            pure uiState
                { currentScreen = Gameplay
                , gameState = Just gs
                }

        Exit ->
            pure uiState { currentScreen = Exit }

        screen ->
            pure uiState { currentScreen = screen }


getClickedButton :: (Float, Float) -> UiScreen -> Maybe Button
getClickedButton mousePos screen =
    findButton mousePos (buttonsForScreen screen)


findButton :: (Float, Float) -> [Button] -> Maybe Button
findButton _ [] = Nothing
findButton mousePos (b:bs)
    | isInsideButton mousePos b = Just b
    | otherwise                = findButton mousePos bs
