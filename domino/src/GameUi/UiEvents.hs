module GameUi.UiEvents where

import Graphics.Gloss.Interface.IO.Game
import GameUi.UiState
import GameUi.UiButton
import GameUi.UiBoard(UiTile(..), playerHandTiles)
import Game.Domino (Domino) 
import Game.GameState (GameState, iniciarPartida, estaTerminado, jugadorActual)
import Game.Board (Lado(..))
import Game.Actions (Accion(..), ResultadoAccion(..), ejecutarAccion, accionesDisponibles)
import Game.Player (playerName, playerHand)
import Game.Rules (ResultadoPartida(..))
import Data.List (find)

handleUiEvent :: Event -> UiState -> IO UiState
handleUiEvent event uiState =
    case event of
        EventKey (MouseButton LeftButton) Up _ mousePos ->
            handleMouseClick mousePos uiState
        -- Tecla R para robar
        EventKey (Char 'r') Up _ _ ->
            handleRobar uiState
        -- Tecla P para pasar
        EventKey (Char 'p') Up _ _ ->
            handlePasar uiState
        -- Tecla Escape para deseleccionar
        EventKey (SpecialKey KeyEsc) Up _ _ ->
            pure uiState { selection = NoSelection, message = "" }
        _ ->
            pure uiState


handleMouseClick :: (Float, Float) -> UiState -> IO UiState
handleMouseClick mousePos uiState =
    case currentScreen uiState of
        Gameplay ->
            case selection uiState of
                NoSelection ->
                    trySelectFromHand mousePos uiState
                Selected domino ->
                    tryPlayDomino mousePos domino uiState
        GameOver ->
            -- Click en GameOver vuelve al menú
            pure uiState { currentScreen = MainMenu, gameState = Nothing, winner = Nothing }
        _ ->
            case getClickedButton mousePos (currentScreen uiState) of
                Just button ->
                    handleButtonAction button uiState
                Nothing ->
                    pure uiState


-- | Intenta jugar la ficha seleccionada en el lado clickeado
tryPlayDomino :: (Float, Float) -> Domino -> UiState -> IO UiState
tryPlayDomino (mx, _) domino uiState =
    case gameState uiState of
        Nothing -> pure uiState
        Just gs -> do
            -- Determinar lado según posición X del click
            let lado = if mx < 0 then Izquierda else Derecha
            let resultado = ejecutarAccion (Jugar domino lado) gs
            procesarResultado resultado uiState


-- | Procesa el resultado de una acción y actualiza el UiState
procesarResultado :: ResultadoAccion -> UiState -> IO UiState
procesarResultado resultado uiState =
    case resultado of
        Exito gs' ->
            pure uiState
                { gameState = Just gs'
                , selection = NoSelection
                , message   = "Turno de: " ++ playerName (jugadorActual gs')
                }
        Victoria jugador gs' ->
            pure uiState
                { gameState     = Just gs'
                , currentScreen = GameOver
                , selection     = NoSelection
                , winner        = Just (playerName jugador ++ " gana!")
                , message       = ""
                }
        Trancado res gs' ->
            let winnerMsg = case res of
                    GanadorDomino j   -> playerName j ++ " gana por domino!"
                    GanadorTrancado j -> playerName j ++ " gana (menos puntos)!"
                    Empate js         -> "Empate entre: " ++ unwords (map playerName js)
            in pure uiState
                { gameState     = Just gs'
                , currentScreen = GameOver
                , selection     = NoSelection
                , winner        = Just winnerMsg
                , message       = ""
                }
        ErrorAccion msg ->
            pure uiState
                { selection = NoSelection
                , message   = "Error: " ++ msg
                }


-- | Manejar robar del pozo
handleRobar :: UiState -> IO UiState
handleRobar uiState =
    case gameState uiState of
        Nothing -> pure uiState
        Just gs -> procesarResultado (ejecutarAccion Robar gs) uiState


-- | Manejar pasar turno
handlePasar :: UiState -> IO UiState
handlePasar uiState =
    case gameState uiState of
        Nothing -> pure uiState
        Just gs -> procesarResultado (ejecutarAccion Pasar gs) uiState


handleButtonAction :: Button -> UiState -> IO UiState
handleButtonAction button uiState =
    case nextScreen button of
        Gameplay -> do
            gs <- iniciarPartida ["Jugador 1", "Jugador 2", "Jugador 3", "Jugador 4"] 10
            pure uiState
                { currentScreen = Gameplay
                , gameState     = Just gs
                , selection     = NoSelection
                , message       = "Turno de: " ++ playerName (jugadorActual gs)
                , winner        = Nothing
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
    | otherwise                 = findButton mousePos bs

isInsideTile :: (Float, Float) -> UiTile -> Bool
isInsideTile (mx, my) (UiTile _ (x, y)) =
    mx >= x - halfW && mx <= x + halfW &&
    my >= y - halfH && my <= y + halfH
  where
    halfW = 20
    halfH = 40

trySelectFromHand :: (Float, Float) -> UiState -> IO UiState
trySelectFromHand mousePos uiState =
    case gameState uiState of
        Just gs -> do
            case find (isInsideTile mousePos) (playerHandTiles gs) of
                Just tile -> 
                    pure uiState 
                        { selection = Selected (uiDomino tile)
                        , message   = "Ficha seleccionada. Click izq/der del tablero para jugar."
                        }
                Nothing ->
                    pure uiState { selection = NoSelection }
        Nothing ->
            pure uiState