module Main where

import System.Environment (getArgs)
import Graphics.Gloss.Interface.IO.Game

import GameUi.UiState
import GameUi.UiRender
import GameUi.UiEvents
import Web.Server (runWebServer)

-- | Window configuration
window :: Display
window = InWindow
    "Domino Game"
    (800, 600)
    (100, 100)

-- | Background color
backgroundColor :: Color
backgroundColor = black

-- | Frames per second
fps :: Int
fps = 60

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["web"] -> runWebServer
        ["visual"] -> runVisual
        _ -> do
            putStrLn "Uso: domino-exe [web|visual]"
            putStrLn "  web    - Servidor web en http://localhost:3000"
            putStrLn "  visual - Interfaz gráfica con Gloss"
            putStrLn ""
            putStrLn "Iniciando modo web por defecto..."
            runWebServer

runVisual :: IO ()
runVisual =
    playIO
        window
        backgroundColor
        fps
        initialUiState
        (pure . renderUi)
        handleUiEvent
        updateUi

updateUi :: Float -> UiState -> IO UiState
updateUi _ = pure
