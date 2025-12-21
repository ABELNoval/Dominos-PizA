module GameUi.UiRender where

import Graphics.Gloss
import GameUi.UiState
import GameUi.UiButton

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
        Gameplay       -> drawGameplayPlaceholder
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
labelOffsetX text =
    fromIntegral (length text) * 6


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
-- Gameplay placeholder
-- =========================================================

drawGameplayPlaceholder :: Picture
drawGameplayPlaceholder =
    Translate (-180) 0 $
        Scale 0.2 0.2 $
            Color white $
                Text "Gameplay Screen"
