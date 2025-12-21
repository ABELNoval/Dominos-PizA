module GameUi.UiHand
  ( drawVisibleHand
  , drawHiddenHand
  ) where

import Graphics.Gloss
import GameUi.UiDomino
import Game.Domino (Domino(..))

------------------------------------------------------------
-- Constants (layout tuning)
------------------------------------------------------------

dominoSpacing :: Float
dominoSpacing = 50

------------------------------------------------------------
-- Public API
------------------------------------------------------------

-- | Draws a hand of visible dominoes (player hand)
-- | Each domino is given as (topValue, bottomValue)
drawVisibleHand :: [Domino] -> Picture
drawVisibleHand dominoes =
  Pictures $
    zipWith drawOne [0..] dominoes
  where
    drawOne index (Domino a b) =
      translate (handOffset index) 0 $
        drawDominoVertical (Domino a b)

-- | Draws a hand of hidden dominoes (opponent hand)
drawHiddenHand :: Int -> Picture
drawHiddenHand count =
  Pictures $
    map drawOne [0 .. count - 1]
  where
    drawOne index =
      translate (handOffset index) 0 drawDominoBack

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

-- | Centers the hand horizontally
handOffset :: Int -> Float
handOffset index =
  fromIntegral index * dominoSpacing
