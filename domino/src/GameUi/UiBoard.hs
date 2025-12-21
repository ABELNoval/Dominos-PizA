module GameUi.UiBoard where

import Graphics.Gloss
import Game.Board (Board, fichasEnMesa)
import Game.Domino (Domino(..))
import Game.Player (Player(..))
import Game.GameState (GameState, jugadorActual)

data UiTile = UiTile {
    uiDomino :: Domino,
    uiPos :: (Float, Float)
}

-- =========================================================
-- Constants
-- =========================================================

dominoWidth, dominoHeight, dominoGap :: Float
dominoWidth  = 40
dominoHeight = 80
dominoGap    = 5

-- =========================================================
-- Board rendering
-- =========================================================

drawBoard :: Board -> Picture
drawBoard board =
    Pictures $
        zipWith drawDominoAt positions (fichasEnMesa board)
  where
    positions = [ (fromIntegral i * (dominoWidth + dominoGap), 0)
                | i <- centeredIndices (length (fichasEnMesa board))
                ]

centeredIndices :: Int -> [Int]
centeredIndices n =
    let half = n `div` 2
    in [i - half | i <- [0 .. n - 1]]

drawDominoAt :: (Float, Float) -> Domino -> Picture
drawDominoAt (x, y) (Domino a b) =
    Translate x y $
        Pictures
            [ drawDominoBase
            , drawDominoDivider
            , drawPips (-(dominoHeight / 4)) a
            , drawPips ( dominoHeight / 4) b
            ]

drawDominoBase :: Picture
drawDominoBase =
    Color white $
        rectangleSolid dominoWidth dominoHeight

drawDominoDivider :: Picture
drawDominoDivider =
    Color black $
        rectangleSolid dominoWidth 2

-- =========================================================
-- Pips (0–9 real domino layout)
-- =========================================================

drawPips :: Float -> Int -> Picture
drawPips y value =
    Translate 0 y $
        Pictures $
            map drawPip (pipPositions value)

drawPip :: (Float, Float) -> Picture
drawPip (x, y) =
    Translate x y $
        Color black $
            circleSolid 3

pipPositions :: Int -> [(Float, Float)]
pipPositions n =
    case n of
        0 -> []
        1 -> [(0, 0)]
        2 -> [(-10, -10), (10, 10)]
        3 -> [(-10, -10), (0, 0), (10, 10)]
        4 -> [(-10, -10), (-10, 10), (10, -10), (10, 10)]
        5 -> [(-10, -10), (-10, 10), (0, 0), (10, -10), (10, 10)]
        6 -> [(-10, -10), (-10, 0), (-10, 10), (10, -10), (10, 0), (10, 10)]
        7 -> pipPositions 6 ++ [(0, 0)]
        8 -> pipPositions 6 ++ [(0, -10), (0, 10)]
        9 -> pipPositions 8 ++ [(0, 0)]
        _ -> []


-- =========================================================
-- Hands rendering
-- =========================================================

drawHands :: [Player] -> Int -> Picture
drawHands players current =
    Pictures $
        zipWith drawPlayerHand [0..] players
  where
    drawPlayerHand idx player
        | idx == current = drawVisibleHand player
        | otherwise      = drawHiddenHand (length (playerHand player))

drawVisibleHand :: Player -> Picture
drawVisibleHand player =
    Translate 0 (-200) $
        Pictures $
            zipWith drawHandDomino [0..] (playerHand player)

drawHandDomino :: Int -> Domino -> Picture
drawHandDomino i domino =
    Translate (fromIntegral i * (dominoWidth + 10)) 0 $
        drawDominoAt (0,0) domino

drawHiddenHand :: Int -> Picture
drawHiddenHand n =
    Translate 0 200 $
        Pictures
            [ Translate (fromIntegral i * 20) 0 hiddenDomino
            | i <- [0 .. n - 1]
            ]

hiddenDomino :: Picture
hiddenDomino =
    Color (makeColorI 220 190 160 255) $
        rectangleSolid dominoWidth dominoHeight

playerHandTiles :: GameState -> [UiTile]
playerHandTiles gs =
  let player = jugadorActual gs
      hand = playerHand player
      startX = - ((fromIntegral (length hand) * (dominoWidth + dominoGap)) / 2)
      stepX = dominoWidth + dominoGap
      y = 200
  in zipWith (\i domino -> UiTile domino (startX + fromIntegral i * stepX, y)) [0..] hand