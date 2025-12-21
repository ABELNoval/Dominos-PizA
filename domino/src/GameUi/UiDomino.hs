module GameUi.UiDomino
  ( drawDominoVertical
  , drawDominoHorizontal
  , drawDominoBack
  ) where

import Graphics.Gloss
import Game.Domino (Domino(..))

------------------------------------------------------------
-- Constants (visual tuning)
------------------------------------------------------------

dominoWidth :: Float
dominoWidth = 40

dominoHeight :: Float
dominoHeight = 80

pipRadius :: Float
pipRadius = 3.5

pipOffset :: Float
pipOffset = 10

------------------------------------------------------------
-- Public API
------------------------------------------------------------

-- | Draws a vertical domino with visible values
drawDominoVertical :: Domino -> Picture
drawDominoVertical (Domino top bottom) =
  Pictures
    [ dominoBase
    , dividerLine
    , translate 0 (dominoHeight / 4)  (drawPips top)
    , translate 0 (-(dominoHeight / 4)) (drawPips bottom)
    ]

-- | Draws a face-down domino (for opponent hands)
drawDominoBack :: Picture
drawDominoBack =
  Pictures
    [ color (makeColorI 220 190 150 255) $
        rectangleSolid dominoWidth dominoHeight
    , color black $
        rectangleWire dominoWidth dominoHeight
    ]

drawDominoHorizontal :: Int -> Int -> Picture
drawDominoHorizontal _ _ =
  Color white $ rectangleSolid 60 30

------------------------------------------------------------
-- Base domino shape
------------------------------------------------------------

dominoBase :: Picture
dominoBase =
  Pictures
    [ color white $
        rectangleSolid dominoWidth dominoHeight
    , color black $
        rectangleWire dominoWidth dominoHeight
    ]

dividerLine :: Picture
dividerLine =
  color black $
    rectangleSolid dominoWidth 2

------------------------------------------------------------
-- Pips (dots)
------------------------------------------------------------

drawPips :: Int -> Picture
drawPips n =
  Pictures $ map drawPip (pipPositions n)

drawPip :: (Float, Float) -> Picture
drawPip (x, y) =
  translate x y $
    color black $
      circleSolid pipRadius

------------------------------------------------------------
-- Pip layouts (classic domino patterns)
------------------------------------------------------------

pipPositions :: Int -> [(Float, Float)]
pipPositions n =
  case n of
    0 -> []

    1 -> [(0, 0)]

    2 -> [(-pipOffset, pipOffset), (pipOffset, -pipOffset)]

    3 -> [(-pipOffset, pipOffset), (0, 0), (pipOffset, -pipOffset)]

    4 -> [(-pipOffset, pipOffset), (pipOffset, pipOffset),
          (-pipOffset, -pipOffset), (pipOffset, -pipOffset)]

    5 -> [(-pipOffset, pipOffset), (pipOffset, pipOffset),
          (0, 0),
          (-pipOffset, -pipOffset), (pipOffset, -pipOffset)]

    6 -> [(-pipOffset, pipOffset), (-pipOffset, 0), (-pipOffset, -pipOffset),
          ( pipOffset, pipOffset), ( pipOffset, 0), ( pipOffset, -pipOffset)]

    7 -> pipPositions 6 ++ [(0, pipOffset)]

    8 -> pipPositions 6 ++ [(0, pipOffset), (0, -pipOffset)]

    9 -> pipPositions 8 ++ [(0, 0)]

    _ -> []  -- safety fallback
