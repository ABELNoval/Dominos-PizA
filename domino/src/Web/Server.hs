{-# LANGUAGE OverloadedStrings #-}

module Web.Server 
    ( runWebServer
    ) where

import Web.Scotty
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Network.HTTP.Types.Status (status400)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.String (fromString)

import Web.GameApi
import Web.Views
import Game.Domino (Domino(..))
import Game.GameState (estaTerminado, getJugadores, getTablero, getPozo, gsTurnoActual)
import Game.Player (playerHand)
import Game.Board (Lado(..))
import Game.Rules (jugadasPosibles)

-- | Ejecutar servidor web en puerto 3000
runWebServer :: IO ()
runWebServer = do
    putStrLn "==================================="
    putStrLn "  🎲 Dominó Web Server"
    putStrLn "  Abre http://localhost:3000"
    putStrLn "==================================="
    
    -- Estado del juego compartido (usando STM para thread-safety)
    initialGame <- createNewGame Facil
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
            let difficulty = case eitherDecode reqBody of
                    Left _ -> Facil  -- Default si no se puede parsear
                    Right diff -> diff
            newSession <- liftIO $ createNewGame difficulty
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
        
        -- API: Jugada automática del bot
        post "/api/bot-play" $ do
            session <- liftIO $ atomically $ do
                current <- readTVar gameVar
                let estado = gsState current
                    turno = gsTurnoActual estado
                if estaTerminado estado || turno == 0
                    then return current
                    else do
                        let jugadores = getJugadores estado
                            jugador = jugadores !! turno
                            mano = playerHand jugador
                            tablero = getTablero estado
                            jugadas = jugadasPosibles mano tablero
                            pozo = getPozo estado
                            newSession = case jugadas of
                                ((d, l):_) -> executeGameAction current (GameAction "play" (Just $ dominoToJson' d) (Just $ sideToText l))
                                [] -> if null pozo
                                      then executeGameAction current (GameAction "pass" Nothing Nothing)
                                      else executeGameAction current (GameAction "draw" Nothing Nothing)
                        writeTVar gameVar newSession
                        return newSession
            json $ gameStateToJson session
  where
    dominoToJson' (Domino a b) = DominoJson a b
    
    sideToText :: Lado -> T.Text
    sideToText Izquierda = "left"
    sideToText Derecha = "right"
