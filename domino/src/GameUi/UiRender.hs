module GameUi.UiRender where

import Graphics.Gloss
import GameUi.UiState
import GameUi.UiButton
import GameUi.UiBoard
import GameUi.UiHand
import Game.GameState (GameState, gsTablero, gsJugadores, gsTurnoActual, gsPozo)
import Game.Player (playerHand)
import qualified GameUi.UiDomino as UiDomino


-- | Main render function for the UI.
renderUi :: UiState -> Picture
renderUi uiState =
    case currentScreen uiState of
        MainMenu       -> Pictures [ drawTitle
                                   , drawMenuWithLabels mainMenuButtons mainMenuLabels
                                   ]
        GameModeMenu   -> Pictures [ drawTitle
                                   , drawMenuWithLabels gameModeMenuButtons gameModeLabels
                                   ]
        DifficultyMenu -> Pictures [ drawTitle
                                   , drawMenuWithLabels difficultyMenuButtons difficultyLabels
                                   ]
        Gameplay       -> 
            maybe Blank (drawGameplay uiState) (gameState uiState)
        GameOver       ->
            drawGameOver uiState
        Exit           -> Blank


-- =========================================================
-- Title
-- =========================================================

-- | Draws the main title of the game.
drawTitle :: Picture
drawTitle =
    Translate (-220) 220 $
        Scale 0.4 0.4 $
            Color white $
                Text "DOMINO's PizA"


-- =========================================================
-- Menus with labels
-- =========================================================

drawMenuWithLabels :: [Button] -> [String] -> Picture
drawMenuWithLabels buttons labels =
    Pictures (zipWith drawButtonWithLabel buttons labels)


drawButtonWithLabel :: Button -> String -> Picture
drawButtonWithLabel button label =
    Pictures
        [ drawButton button
        , drawButtonLabel button label
        ]


-- =========================================================
-- Button drawing
-- =========================================================

drawButton :: Button -> Picture
drawButton button =
    Translate (buttonX button) (buttonY button) $
        Color white $
            rectangleSolid
                (buttonWidth button)
                (buttonHeight button)


drawButtonLabel :: Button -> String -> Picture
drawButtonLabel button label =
    Translate (buttonX button - labelOffsetX label)
              (buttonY button - 10) $
        Scale 0.15 0.15 $
            Color black $
                Text label


-- | Small helper to roughly center text horizontally
labelOffsetX :: String -> Float
labelOffsetX txt =
    fromIntegral (length txt) * 6


-- =========================================================
-- Labels per menu
-- =========================================================

mainMenuLabels :: [String]
mainMenuLabels =
    [ "PLAY"
    , "EXIT"
    ]


gameModeLabels :: [String]
gameModeLabels =
    [ "VS AI"
    ]


difficultyLabels :: [String]
difficultyLabels =
    [ "EASY"
    , "MEDIUM"
    , "HARD"
    ]


-- =========================================================
-- Gameplay screen
-- =========================================================

drawGameplay :: UiState -> GameState -> Picture
drawGameplay uiState gs =
    Pictures
        [ drawBoard (gsTablero gs)
        , drawHands (gsJugadores gs) (gsTurnoActual gs)
        , drawSelectedHighlight uiState gs
        , drawMessage (message uiState)
        , drawControls
        , drawPozoCount (length (gsPozo gs))
        , drawPlayZones
        ]


-- | Dibuja las zonas clickeables para jugar izq/der
drawPlayZones :: Picture
drawPlayZones =
    Pictures
        [ Translate (-300) 0 $ Color (makeColorI 0 100 0 50) $ rectangleSolid 100 200
        , Translate (-300) 0 $ Color (makeColorI 0 255 0 150) $ 
            Translate (-30) 0 $ Scale 0.1 0.1 $ Text "IZQ"
        , Translate 300 0 $ Color (makeColorI 0 100 0 50) $ rectangleSolid 100 200
        , Translate 300 0 $ Color (makeColorI 0 255 0 150) $ 
            Translate (-30) 0 $ Scale 0.1 0.1 $ Text "DER"
        ]


-- | Resalta la ficha seleccionada
drawSelectedHighlight :: UiState -> GameState -> Picture
drawSelectedHighlight uiState gs =
    case selection uiState of
        NoSelection -> Blank
        Selected domino ->
            let tiles = playerHandTiles gs
                matching = filter (\t -> uiDomino t == domino) tiles
            in Pictures $ map highlightTile matching
  where
    highlightTile (UiTile _ (x, y)) =
        Translate x y $ Color green $ rectangleWire 44 84


-- | Mensaje en pantalla (errores, info)
drawMessage :: String -> Picture
drawMessage msg =
    Translate (-380) 250 $
        Scale 0.12 0.12 $
            Color yellow $
                Text msg


-- | Controles en pantalla
drawControls :: Picture
drawControls =
    Translate (-380) (-270) $
        Scale 0.1 0.1 $
            Color (greyN 0.7) $
                Text "R: Robar | P: Pasar | ESC: Deseleccionar | Click ficha + Click IZQ/DER"


-- | Cantidad de fichas en el pozo
drawPozoCount :: Int -> Picture
drawPozoCount n =
    Translate 300 250 $
        Scale 0.12 0.12 $
            Color cyan $
                Text ("Pozo: " ++ show n)


drawHandTile :: UiSelection -> UiTile -> Picture
drawHandTile sel (UiTile d (x, y)) =
    Translate x y $
        Pictures
            [ if isSelected then highlight else Blank
            , UiDomino.drawDominoVertical d
            ]
  where
    isSelected = sel == Selected d
    highlight  = Color green $ rectangleWire 44 84


-- =========================================================
-- Game Over screen
-- =========================================================

drawGameOver :: UiState -> Picture
drawGameOver uiState =
    Pictures
        [ Color (makeColorI 0 0 0 200) $ rectangleSolid 800 600
        , Translate (-180) 100 $
            Scale 0.4 0.4 $
                Color white $
                    Text "GAME OVER"
        , Translate (-200) 0 $
            Scale 0.2 0.2 $
                Color green $
                    Text (maybe "Empate" id (winner uiState))
        , Translate (-150) (-100) $
            Scale 0.15 0.15 $
                Color (greyN 0.7) $
                    Text "Click para volver al menu"
        ]


-- =========================================================
-- Temporary mock data (will come from Game later)
-- =========================================================

-- boardDominoes :: [(Int, Int)]
-- boardDominoes =
--     [ (6,6)
--     , (6,4)
--     , (4,1)
--     , (1,3)
--     ]

-- playerHand :: [(Int, Int)]
-- playerHand =
--     [ (0,1)
--     , (2,5)
--     , (3,3)
--     , (6,2)
--     , (4,4)
--     ]

-- opponentHandSize :: Int
-- opponentHandSize = 7
