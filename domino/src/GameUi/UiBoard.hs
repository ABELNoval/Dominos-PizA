module GameUi.UiBoard where

import Graphics.Gloss
import Game.Board (Board, fichasEnMesa)
import Game.Domino (Domino(..), esDoble)
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

-- Tamaño de ficha horizontal (rotada 90°)
dominoHorizWidth, dominoHorizHeight :: Float
dominoHorizWidth  = dominoHeight  -- 80
dominoHorizHeight = dominoWidth   -- 40

-- Máximo de fichas antes de doblar
maxFichasAntesDoblar :: Int
maxFichasAntesDoblar = 8

-- =========================================================
-- Board rendering (estilo dominó real)
-- =========================================================

-- | Dirección en la que crece la cadena
data Direccion = Derecha | Izquierda | Arriba | Abajo
    deriving (Eq, Show)

-- | Estado del dibujo del tablero
-- (posición actual, dirección, contador de fichas en esta línea, esPrimeraFicha, pictures)
type EstadoTablero = ((Float, Float), Direccion, Int, Bool, [Picture])

-- | Extrae las pictures del estado
getPictures :: EstadoTablero -> [Picture]
getPictures (_, _, _, _, pics) = pics

drawBoard :: Board -> Picture
drawBoard board =
    Translate 0 100 $  -- Centrar verticalmente en la pantalla
    Pictures $ getPictures $ foldl colocarFicha estadoInicial (fichasEnMesa board)
  where
    estadoInicial :: EstadoTablero
    estadoInicial = ((0, 0), Derecha, 0, True, [])

-- | Coloca una ficha y devuelve el nuevo estado
colocarFicha :: EstadoTablero -> Domino -> EstadoTablero
colocarFicha ((x, y), dir, count, esPrimera, pics) domino =
    let esDobleF = esDoble domino
        
        -- Si es la primera ficha, se centra en (0,0)
        -- Si no, se coloca según la dirección
        
        -- Decidir si hay que doblar
        debeDoblar = count >= maxFichasAntesDoblar && not esPrimera
        
        -- Nueva dirección (solo cambia si doblamos)
        nuevaDir = if debeDoblar then doblarDireccion dir else dir
        
        -- Reiniciar contador si doblamos
        nuevoCount = if debeDoblar then 1 else count + 1
        
        -- Dibujar la ficha
        pic = dibujarFichaEnMesa domino (x, y) nuevaDir esDobleF
        
        -- Calcular siguiente posición
        siguientePos = calcularSiguientePosicion (x, y) nuevaDir esDobleF
        
    in (siguientePos, nuevaDir, nuevoCount, False, pics ++ [pic])

-- | Dobla la dirección (Derecha→Abajo→Izquierda→Arriba→...)
doblarDireccion :: Direccion -> Direccion
doblarDireccion Derecha   = Abajo
doblarDireccion Abajo     = Izquierda
doblarDireccion Izquierda = Arriba
doblarDireccion Arriba    = Derecha

-- | Dibuja una ficha según si es doble o no y la dirección
dibujarFichaEnMesa :: Domino -> (Float, Float) -> Direccion -> Bool -> Picture
dibujarFichaEnMesa domino (x, y) dir esDobleF
    -- Los dobles se dibujan perpendiculares a la dirección de la cadena
    | esDobleF  = Translate x y $ rotarDobleSegunDir dir $ drawDominoVertical domino
    -- Las fichas normales se dibujan horizontales en la dirección de la cadena
    | otherwise = Translate x y $ rotarSegunDir dir $ drawDominoHorizontal domino

-- | Rota la ficha normal según la dirección de la cadena
rotarSegunDir :: Direccion -> Picture -> Picture
rotarSegunDir Derecha   = id           -- Horizontal, apunta derecha
rotarSegunDir Izquierda = Rotate 180   -- Horizontal, apunta izquierda
rotarSegunDir Abajo     = Rotate 90    -- Vertical, apunta abajo
rotarSegunDir Arriba    = Rotate (-90) -- Vertical, apunta arriba

-- | Rota el doble para que sea perpendicular a la cadena
rotarDobleSegunDir :: Direccion -> Picture -> Picture
rotarDobleSegunDir Derecha   = id           -- Doble vertical cuando cadena va horizontal
rotarDobleSegunDir Izquierda = id           -- Doble vertical cuando cadena va horizontal
rotarDobleSegunDir Abajo     = Rotate 90    -- Doble horizontal cuando cadena va vertical
rotarDobleSegunDir Arriba    = Rotate 90    -- Doble horizontal cuando cadena va vertical

-- | Calcula la siguiente posición según dirección y tipo de ficha
calcularSiguientePosicion :: (Float, Float) -> Direccion -> Bool -> (Float, Float)
calcularSiguientePosicion (x, y) dir esDobleF =
    let -- El step depende de la orientación de la ficha
        stepHoriz = dominoHorizWidth + dominoGap   -- Paso cuando la cadena va horizontal
        stepVert  = dominoHorizWidth + dominoGap   -- Paso cuando la cadena va vertical
        stepDoble = dominoWidth + dominoGap        -- Paso para dobles (son más estrechos)
    in case dir of
        Derecha   -> (x + (if esDobleF then stepDoble else stepHoriz), y)
        Izquierda -> (x - (if esDobleF then stepDoble else stepHoriz), y)
        Abajo     -> (x, y - (if esDobleF then stepDoble else stepVert))
        Arriba    -> (x, y + (if esDobleF then stepDoble else stepVert))

-- | Dibuja una ficha en horizontal (para fichas normales en la mesa)
drawDominoHorizontal :: Domino -> Picture
drawDominoHorizontal (Domino a b) =
    Pictures
        [ Color white $ rectangleSolid dominoHorizWidth dominoHorizHeight
        , Color black $ rectangleSolid 2 dominoHorizHeight  -- Divisor vertical
        , Translate (-dominoHorizWidth/4) 0 $ drawPipsSmall a
        , Translate (dominoHorizWidth/4) 0 $ drawPipsSmall b
        ]

-- | Dibuja una ficha en vertical (para dobles)
drawDominoVertical :: Domino -> Picture
drawDominoVertical (Domino a b) =
    Pictures
        [ Color white $ rectangleSolid dominoWidth dominoHeight
        , Color black $ rectangleSolid dominoWidth 2  -- Divisor horizontal
        , Translate 0 (-dominoHeight/4) $ drawPipsSmall a
        , Translate 0 (dominoHeight/4) $ drawPipsSmall b
        ]

-- | Pips más pequeños para que quepan bien
drawPipsSmall :: Int -> Picture
drawPipsSmall value =
    Pictures $ map drawPipSmall (pipPositionsSmall value)

drawPipSmall :: (Float, Float) -> Picture
drawPipSmall (px, py) =
    Translate px py $ Color black $ circleSolid 2.5

pipPositionsSmall :: Int -> [(Float, Float)]
pipPositionsSmall n =
    case n of
        0 -> []
        1 -> [(0, 0)]
        2 -> [(-6, -6), (6, 6)]
        3 -> [(-6, -6), (0, 0), (6, 6)]
        4 -> [(-6, -6), (-6, 6), (6, -6), (6, 6)]
        5 -> [(-6, -6), (-6, 6), (0, 0), (6, -6), (6, 6)]
        6 -> [(-6, -6), (-6, 0), (-6, 6), (6, -6), (6, 0), (6, 6)]
        7 -> pipPositionsSmall 6 ++ [(0, 0)]
        8 -> pipPositionsSmall 6 ++ [(0, -6), (0, 6)]
        9 -> pipPositionsSmall 8 ++ [(0, 0)]
        _ -> []


-- =========================================================
-- Hands rendering
-- =========================================================

-- Constantes para la mano del jugador
handY :: Float
handY = -200

handStepX :: Float
handStepX = dominoWidth + 10

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
    let hand = playerHand player
        n = length hand
        startX = - (fromIntegral (n - 1) * handStepX / 2)
    in Translate 0 handY $
        Pictures $
            zipWith (drawHandDominoAt startX) [0..] hand

drawHandDominoAt :: Float -> Int -> Domino -> Picture
drawHandDominoAt startX i domino =
    Translate (startX + fromIntegral i * handStepX) 0 $
        drawDominoVertical domino

drawHiddenHand :: Int -> Picture
drawHiddenHand n =
    let startX = - (fromIntegral (n - 1) * hiddenStepX / 2)
        hiddenStepX = dominoWidth + 10
    in Translate 0 200 $
        Pictures
            [ Translate (startX + fromIntegral i * hiddenStepX) 0 hiddenDomino
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
      n = length hand
      startX = - (fromIntegral (n - 1) * handStepX / 2)
  in zipWith (\i domino -> UiTile domino (startX + fromIntegral i * handStepX, handY)) [0..] hand