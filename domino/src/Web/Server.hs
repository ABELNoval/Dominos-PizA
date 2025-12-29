{-# LANGUAGE OverloadedStrings #-}

module Web.Server 
    ( runWebServer
    ) where

import Web.Scotty
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Control.Concurrent.STM
import Data.Aeson (eitherDecode)
import Network.HTTP.Types.Status (status400)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import Web.GameApi
import Web.Views
import Game.Domino (Domino(..))
import Game.GameState (estaTerminado, gsTurnoActual)
import Game.Board (Lado(..))
import Game.AI (chooseBotAction)
import qualified Game.AI as AI
import Game.Actions (Accion(..))

-- | Ejecutar servidor web en puerto 3000
runWebServer :: IO ()
runWebServer = do
    putStrLn "==================================="
    putStrLn "  Domino Web Server"
    putStrLn "  Abre http://localhost:3000"
    putStrLn "==================================="
    
    -- Estado del juego compartido (usando STM para thread-safety)
    let defaultConfig = GameConfig Robadito FreeForAll Easy
    initialGame <- createNewGame defaultConfig
    gameVar <- newTVarIO initialGame
    
    scotty 3000 $ do
        -- Middleware para archivos estáticos (si los hay)
        middleware $ staticPolicy (addBase "static")
        
        -- Página principal
        get "/" $ do
            html indexHtml
        
        -- Página del juego
        get "/game" $ do
            html gameHtml
        
        -- API: Obtener estado actual
        get "/api/state" $ do
            session <- liftIO $ readTVarIO gameVar
            json $ gameStateToJson session
        
        -- API: Nueva partida
        post "/api/new" $ do
            reqBody <- body
            let config = case eitherDecode reqBody of
                    Left _ -> GameConfig Robadito FreeForAll Easy
                    Right cfg -> cfg
            newSession <- liftIO $ createNewGame config
            liftIO $ atomically $ writeTVar gameVar newSession
            json $ gameStateToJson newSession
        
        -- API: Realizar jugada
        post "/api/play" $ do
            reqBody <- body
            case eitherDecode reqBody of
                Left err -> do
                    status status400
                    text $ "Error: " <> TL.pack err
                Right action -> do
                    session <- liftIO $ atomically $ do
                        current <- readTVar gameVar
                        let newSession = executeGameAction current action
                        writeTVar gameVar newSession
                        return newSession
                    json $ gameStateToJson session
        
        -- API: Jugada automática del bot usando la IA apropiada
        post "/api/bot-play" $ do
            session <- liftIO $ atomically $ do
                current <- readTVar gameVar
                let estado = gsState current
                    turno = gsTurnoActual estado
                    difficulty = gsDifficulty current
                    gameMode = gsGameMode current
                if estaTerminado estado || turno == 0
                    then return current
                    else do
                        -- Convertir dificultad del API a dificultad de la IA
                        let aiDiff = case difficulty of
                              Easy -> AI.Easy
                              Medium -> AI.Medium
                              Hard -> AI.Hard
                        -- Obtener la acción del bot usando la IA
                        let accion = chooseBotAction aiDiff estado
                        -- Convertir la acción a GameAction
                        let gameAction = case accion of
                              Jugar d l -> GameAction "play" (Just $ dominoToJson' d) (Just $ sideToText l)
                              Pasar -> GameAction "pass" Nothing Nothing
                              Robar -> 
                                -- En modo NoRobadito, el bot pasa en lugar de robar
                                case gameMode of
                                  NoRobadito -> GameAction "pass" Nothing Nothing
                                  Robadito -> GameAction "draw" Nothing Nothing
                        let newSession = executeGameAction current gameAction
                        writeTVar gameVar newSession
                        return newSession
            json $ gameStateToJson session
  where
    dominoToJson' (Domino a b) = DominoJson a b
    
    sideToText :: Lado -> T.Text
    sideToText Izquierda = "left"
    sideToText Derecha = "right"
