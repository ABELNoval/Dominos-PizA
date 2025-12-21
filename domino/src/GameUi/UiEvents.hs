module GameUi.UiEvents where

import Graphics.Gloss.Interface.IO.Game
import GameUi.UiState
import GameUi.UiButton
import GameUi.UiBoard(UiTile(..), playerHandTiles)
import Game.Domino (Domino) 
import Game.GameState (iniciarPartida)
import Data.List (find)

handleUiEvent :: Event -> UiState -> IO UiState
handleUiEvent event uiState =
    case event of
        EventKey (MouseButton LeftButton) Up _ mousePos ->
            handleMouseClick mousePos uiState
        _ ->
            pure uiState


handleMouseClick :: (Float, Float) -> UiState -> IO UiState
handleMouseClick mousePos uiState =
    case currentScreen uiState of
        Gameplay ->
            case selection uiState of
                NoSelection ->
                    trySelectFormHand mousePos uiState
                Selected _ ->
                    pure uiState
        _ ->
            case getClickedButton mousePos (currentScreen uiState) of
                Just button ->
                    handleButtonAction button uiState
                Nothing ->
                    pure uiState


handleButtonAction :: Button -> UiState -> IO UiState
handleButtonAction button uiState =
    case nextScreen button of
        Gameplay -> do
            gs <- iniciarPartida ["Player 1", "Player 2"] 7
            pure uiState
                { currentScreen = Gameplay
                , gameState     = Just gs
                , selection     = NoSelection
                }

        Exit ->
            pure uiState { currentScreen = Exit }

        screen ->
            pure uiState
                { currentScreen = screen
                , selection     = NoSelection
                }


getClickedButton :: (Float, Float) -> UiScreen -> Maybe Button
getClickedButton mousePos screen =
    findButton mousePos (buttonsForScreen screen)


findButton :: (Float, Float) -> [Button] -> Maybe Button
findButton _ [] = Nothing
findButton mousePos (b:bs)
    | isInsideButton mousePos b = Just b
    | otherwise                = findButton mousePos bs

isInsideTile :: (Float, Float) -> UiTile -> Bool
isInsideTile (mx, my) (UiTile _ (x, y)) =
    mx >= x && mx <= x + tileWidth &&
    my >= y && my <= y + tileHeight
  where
    tileWidth = 20
    tileHeight = 40

trySelectFormHand :: (Float, Float) -> UiState -> IO UiState
trySelectFormHand mousePos uiState =
    case gameState uiState of
        Just gs -> do
            case find (isInsideTile mousePos) (playerHandTiles gs) of
                Just tile -> 
                    pure uiState { selection = Selected (uiDomino tile) }
                Nothing ->
                    pure uiState { selection = NoSelection }
        Nothing ->
            pure uiState