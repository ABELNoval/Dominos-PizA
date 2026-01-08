{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.GameApi 
    ( GameSession(..)
    , GameAction(..)
    , GameResponse(..)
    , GameConfig(..)
    , GameMode(..)
    , VsMode(..)
    , Difficulty(..)
    , DataA100Config(..)
    , MatchState(..)
    , DominoJson(..)
    , PlayerJson(..)
    , BoardJson(..)
    , createNewGame
    , executeGameAction
    , gameStateToJson
    , startNextRound
    , defaultDataA100Config
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), (.:?), (.!=), withObject)
import qualified Data.Text as T

import Game.Domino (Domino(..))
import Game.Board (Board, Lado(..), extremoIzquierdo, extremoDerecho, fichasEnMesa)
import Game.Player (Player(..), Team(..), playerHand)
import Game.GameState (GameState(..), FaseJuego(..), iniciarPartida, iniciarPartidaEquipos, jugadorActual, estaTerminado, getTablero, getJugadores, getPozo, siguienteTurno)
import Game.Actions (Accion(..), ResultadoAccion(..), ejecutarAccion)
import Game.Rules (ResultadoPartida(..), ResultadoEquipos(..), puedeJugar, determinarGanador, determinarGanadorEquipos)

-- =========================================================
-- JSON Data Types
-- =========================================================

data DominoJson = DominoJson
    { djLeft  :: Int
    , djRight :: Int
    } deriving (Show, Generic)

instance ToJSON DominoJson where
    toJSON (DominoJson l r) = object ["left" .= l, "right" .= r]

instance FromJSON DominoJson where
    parseJSON = withObject "DominoJson" $ \v ->
        DominoJson <$> v .: "left" <*> v .: "right"

data PlayerJson = PlayerJson
    { pjName :: T.Text
    , pjHand :: [DominoJson]
    , pjHandCount :: Int
    , pjTeam :: T.Text
    } deriving (Show, Generic)

instance ToJSON PlayerJson

-- | Una ficha posicionada en el tablero para JSON
data TileJson = TileJson
    { tjLeft :: Int
    , tjRight :: Int
    , tjX :: Int
    , tjY :: Int
    , tjIsVertical :: Bool
    , tjRotation :: Int  -- Rotación en grados (0, 90, 180, 270)
    } deriving (Show, Generic)

instance ToJSON TileJson where
    toJSON (TileJson l r x y v rot) = object 
        ["left" .= l, "right" .= r, "x" .= x, "y" .= y, "isVertical" .= v, "rotation" .= rot]

data BoardJson = BoardJson
    { bjTiles :: [TileJson]  -- Ahora incluye posiciones
    , bjLeftEnd :: Maybe Int
    , bjRightEnd :: Maybe Int
    } deriving (Show, Generic)

instance ToJSON BoardJson

data GameResponse = GameResponse
    { grBoard :: BoardJson
    , grPlayers :: [PlayerJson]
    , grCurrentPlayer :: Int
    , grCurrentPlayerName :: T.Text
    , grMessage :: T.Text
    , grGameOver :: Bool
    , grWinner :: Maybe T.Text
    , grPozoCount :: Int
    , grCanDraw :: Bool
    , grGameMode :: T.Text
    , grVsMode :: T.Text
    , grDifficulty :: T.Text
    , grMatchState :: Maybe MatchStateJson  -- Estado de Data a 100
    } deriving (Show, Generic)

instance ToJSON GameResponse

-- | Estado de la partida Data a 100 para JSON
data MatchStateJson = MatchStateJson
    { msjTeamAScore :: Int
    , msjTeamBScore :: Int
    , msjRoundNumber :: Int
    , msjTargetScore :: Int
    , msjMatchOver :: Bool
    , msjMatchWinner :: Maybe T.Text
    , msjIsFirstRound :: Bool
    , msjBonusNoSalida :: Bool
    , msjBonusPrimerDoble :: Bool
    , msjBonusCapicua :: Bool
    } deriving (Show, Generic)

instance ToJSON MatchStateJson

data GameAction = GameAction
    { gaAction :: T.Text  -- "play", "pass", "draw"
    , gaDomino :: Maybe DominoJson
    , gaSide :: Maybe T.Text  -- "left", "right"
    } deriving (Show, Generic)

instance FromJSON GameAction where
    parseJSON = withObject "GameAction" $ \v ->
        GameAction <$> v .: "action" 
                   <*> v .: "domino"
                   <*> v .: "side"

-- =========================================================
-- Game Configuration
-- =========================================================

-- | Modo de juego: si se puede robar o no
data GameMode = Robadito | NoRobadito
    deriving (Show, Eq, Generic)

instance ToJSON GameMode
instance FromJSON GameMode where
    parseJSON = withObject "GameMode" $ \v -> do
        mode <- v .: "gameMode"
        case (mode :: T.Text) of
            "robadito" -> return Robadito
            "norobadito" -> return NoRobadito
            _ -> return Robadito

-- | Modo VS: cuántos jugadores
data VsMode = OneVsOne | FreeForAll | TwoVsTwo
    deriving (Show, Eq, Generic)

instance ToJSON VsMode
instance FromJSON VsMode where
    parseJSON = withObject "VsMode" $ \v -> do
        mode <- v .: "vsMode"
        case (mode :: T.Text) of
            "1vs1" -> return OneVsOne
            "freeforall" -> return FreeForAll
            "2vs2" -> return TwoVsTwo
            _ -> return FreeForAll

-- | Dificultad de la IA
data Difficulty = Easy | Medium | Hard | Extreme
    deriving (Show, Eq, Generic)

instance ToJSON Difficulty
instance FromJSON Difficulty where
    parseJSON = withObject "Difficulty" $ \v -> do
        diff <- v .: "difficulty"
        case (diff :: T.Text) of
            "easy" -> return Easy
            "medium" -> return Medium
            "hard" -> return Hard
            "extreme" -> return Extreme
            _ -> return Easy

-- | Configuración completa del juego
data GameConfig = GameConfig
    { gcGameMode :: GameMode
    , gcVsMode :: VsMode
    , gcDifficulty :: Difficulty
    , gcDataA100 :: Maybe DataA100Config  -- Configuración de Data a 100
    } deriving (Show, Eq, Generic)

-- | Configuración para el modo Data a 100
data DataA100Config = DataA100Config
    { daEnabled :: Bool           -- Si está habilitado el modo Data a 100
    , daTargetScore :: Int        -- Puntuación objetivo (default 100)
    , daBonusNoSalida :: Bool     -- No llevar salida suma 20 puntos
    , daBonusPrimerDoble :: Bool  -- Primer partido cuenta doble
    , daBonusCapicua :: Bool      -- Pegarse capicúa cuenta doble
    } deriving (Show, Eq, Generic)

instance ToJSON DataA100Config
instance FromJSON DataA100Config where
    parseJSON = withObject "DataA100Config" $ \v ->
        DataA100Config
            <$> v .:? "enabled" .!= False
            <*> v .:? "targetScore" .!= 100
            <*> v .:? "bonusNoSalida" .!= False
            <*> v .:? "bonusPrimerDoble" .!= False
            <*> v .:? "bonusCapicua" .!= False

-- | Configuración por defecto de Data a 100
defaultDataA100Config :: DataA100Config
defaultDataA100Config = DataA100Config False 100 False False False

instance FromJSON GameConfig where
    parseJSON = withObject "GameConfig" $ \v -> do
        gm <- v .:? "gameMode" .!= "robadito"
        vs <- v .:? "vsMode" .!= "freeforall"
        diff <- v .:? "difficulty" .!= "easy"
        dataA100 <- v .:? "dataA100"
        let gameMode = case (gm :: T.Text) of
              "robadito" -> Robadito
              "norobadito" -> NoRobadito
              _ -> Robadito
        let vsMode = case (vs :: T.Text) of
              "1vs1" -> OneVsOne
              "freeforall" -> FreeForAll
              "2vs2" -> TwoVsTwo
              _ -> FreeForAll
        let difficulty = case (diff :: T.Text) of
              "easy" -> Easy
              "medium" -> Medium
              "hard" -> Hard
              "extreme" -> Extreme
              _ -> Easy
        return $ GameConfig gameMode vsMode difficulty dataA100

-- =========================================================
-- Match State (Data a 100)
-- =========================================================

-- | Estado de la partida acumulativa (Data a 100)
data MatchState = MatchState
    { msTeamAScore :: Int         -- Puntuación acumulada del Equipo A
    , msTeamBScore :: Int         -- Puntuación acumulada del Equipo B
    , msRoundNumber :: Int        -- Número de ronda actual
    , msConfig :: DataA100Config  -- Configuración de bonuses
    , msMatchOver :: Bool         -- Si la partida terminó
    , msMatchWinner :: Maybe Team -- Equipo ganador de la partida
    , msFirstRoundWinner :: Maybe Team  -- Quién ganó la primera ronda (para bonus)
    , msLastRoundCapicua :: Bool  -- Si la última ronda fue capicúa
    , msLastRoundNoSalida :: Maybe Team -- Equipo que no llevó salida
    , msLastRoundWinner :: Maybe Team   -- Quién ganó la última ronda (para determinar quién empieza)
    } deriving (Show, Eq)

-- | Estado inicial de una partida Data a 100
initialMatchState :: DataA100Config -> MatchState
initialMatchState cfg = MatchState
    { msTeamAScore = 0
    , msTeamBScore = 0
    , msRoundNumber = 1
    , msConfig = cfg
    , msMatchOver = False
    , msMatchWinner = Nothing
    , msFirstRoundWinner = Nothing
    , msLastRoundCapicua = False
    , msLastRoundNoSalida = Nothing
    , msLastRoundWinner = Nothing
    }

-- =========================================================
-- Game Session
-- =========================================================

-- | Historial de fichas jugadas para la IA 2vs2
type PlayedHistory = [(Domino, Int)]

-- | Posición visual de una ficha en el tablero
data VisualTile = VisualTile
    { vtDomino :: Domino
    , vtX :: Int        -- Posición X en píxeles
    , vtY :: Int        -- Posición Y en píxeles  
    , vtIsVertical :: Bool  -- True si la ficha está vertical
    , vtRotation :: Int     -- Rotación en grados (0, 90, 180, 270)
    } deriving (Show, Eq)

-- | Lista de fichas con sus posiciones visuales
type BoardPositions = [VisualTile]

data GameSession = GameSession
    { gsState :: GameState
    , gsSessionMessage :: T.Text
    , gsGameMode :: GameMode
    , gsVsMode :: VsMode
    , gsDifficulty :: Difficulty
    , gsWinner :: Maybe T.Text
    , gsPlayedHistory :: PlayedHistory  -- Historial para IA 2vs2
    , gsMatchState :: Maybe MatchState  -- Estado de Data a 100
    , gsRoundWasCapicua :: Bool         -- Si la ronda actual terminó capicúa
    , gsFirstTileIndex :: Int           -- Índice de la primera ficha jugada (para centrar)
    }

-- =========================================================
-- API Functions
-- =========================================================

-- | Crear un nuevo juego
createNewGame :: GameConfig -> IO GameSession
createNewGame config = do
    let nombres = case gcVsMode config of
            OneVsOne -> ["Tú", "Bot 1"]
            FreeForAll -> ["Tú", "Bot 1", "Bot 2", "Bot 3"]
            TwoVsTwo -> ["Tú", "Rival 1", "Compañero", "Rival 2"]  -- Orden: A, B, A, B
        fichasPorJugador = case gcVsMode config of
            OneVsOne -> 10
            FreeForAll -> 10
            TwoVsTwo -> 10
    estado <- case gcVsMode config of
        TwoVsTwo -> iniciarPartidaEquipos nombres fichasPorJugador
        _        -> iniciarPartida nombres fichasPorJugador
    let jugador = jugadorActual estado
        diffText = case gcDifficulty config of
            Easy -> "Fácil"
            Medium -> "Medio"
            Hard -> "Difícil"
            Extreme -> "Extremo"
        modeText = case gcGameMode config of
            Robadito -> "Robadito"
            NoRobadito -> "Sin robar"
        vsText = case gcVsMode config of
            OneVsOne -> "1 vs 1"
            FreeForAll -> "Todos vs Todos"
            TwoVsTwo -> "2 vs 2 (Equipos)"
        -- Inicializar estado de Data a 100 si está habilitado
        matchState = case gcDataA100 config of
            Just da100Cfg | daEnabled da100Cfg -> Just (initialMatchState da100Cfg)
            _ -> Nothing
        dataA100Text = case matchState of
            Just ms -> " | Data a " ++ show (daTargetScore $ msConfig ms)
            Nothing -> ""
    return $ GameSession 
        estado 
        (T.pack $ "¡Juego iniciado! " ++ modeText ++ " | " ++ vsText ++ " | Dificultad: " ++ diffText ++ dataA100Text ++ ". Turno de " ++ playerName jugador)
        (gcGameMode config)
        (gcVsMode config)
        (gcDifficulty config)
        Nothing
        []  -- Historial vacío al inicio
        matchState
        False  -- No capicúa al inicio
        0      -- Índice de primera ficha (se actualizará cuando se juegue)

-- | Ejecutar una acción del jugador
executeGameAction :: GameSession -> GameAction -> GameSession
executeGameAction session action =
    let estado = gsState session
        gameMode = gsGameMode session
        vsMode = gsVsMode session
        turnoActual = gsTurnoActual estado
        accion = parseAction action gameMode
        tableroAntes = getTablero estado
        tableroVacio = null (fichasEnMesa tableroAntes)
    in case accion of
        Nothing -> session { gsSessionMessage = "Acción inválida" }
        Just acc -> 
            -- Registrar la ficha jugada en el historial si es una jugada
            let historialActualizado = case acc of
                    Jugar ficha _ -> gsPlayedHistory session ++ [(ficha, turnoActual)]
                    _ -> gsPlayedHistory session
                -- Actualizar el índice de la primera ficha
                nuevoIndice = case acc of
                    Jugar _ Izquierda -> 
                        if tableroVacio then 0  -- Primera ficha
                        else gsFirstTileIndex session + 1  -- Se agregó a la izquierda
                    Jugar _ Derecha ->
                        if tableroVacio then 0  -- Primera ficha
                        else gsFirstTileIndex session  -- No cambia
                    _ -> gsFirstTileIndex session
            in 
            -- En modo NoRobadito, usamos una validación especial para Pasar
            case (acc, gameMode) of
                (Pasar, NoRobadito) -> ejecutarPasarNoRobadito session estado
                _ -> case ejecutarAccion acc estado of
                    Exito nuevoEstado -> 
                        let msg = T.pack $ "Turno de " ++ playerName (jugadorActual nuevoEstado)
                        in session { gsState = nuevoEstado, gsSessionMessage = msg, gsWinner = Nothing, gsPlayedHistory = historialActualizado, gsFirstTileIndex = nuevoIndice }
                    Victoria ganador nuevoEstado ->
                        let tablero = getTablero nuevoEstado
                            wasCapicua = esCapicua tablero
                        in case vsMode of
                            TwoVsTwo -> 
                                let winningTeam = playerTeam ganador
                                    losingTeam = if winningTeam == TeamA then TeamB else TeamA
                                    teamName = case winningTeam of
                                        TeamA -> "Equipo A (Tú y Compañero)"
                                        TeamB -> "Equipo B (Rivales)"
                                        NoTeam -> playerName ganador
                                    baseSession = session 
                                        { gsState = nuevoEstado
                                        , gsWinner = Just (T.pack teamName)
                                        , gsPlayedHistory = historialActualizado
                                        , gsRoundWasCapicua = wasCapicua
                                        , gsFirstTileIndex = nuevoIndice
                                        }
                                    -- Actualizar Data a 100 si está activo
                                    finalSession = case gsMatchState session of
                                        Just ms -> 
                                            let updated = finalizeRound baseSession losingTeam wasCapicua
                                                capicuaMsg = if wasCapicua && daBonusCapicua (msConfig ms) then " ¡CAPICÚA!" else ""
                                            in updated { gsSessionMessage = T.pack $ "¡Victoria! " ++ teamName ++ " ganó - " ++ playerName ganador ++ " se quedó sin fichas" ++ capicuaMsg }
                                        Nothing -> baseSession { gsSessionMessage = T.pack $ "¡Victoria! " ++ teamName ++ " ganó - " ++ playerName ganador ++ " se quedó sin fichas" }
                                in finalSession
                            _ ->
                                let msg = T.pack $ "¡Victoria! " ++ playerName ganador ++ " ganó"
                                    winner = T.pack $ playerName ganador
                                in session { gsState = nuevoEstado, gsSessionMessage = msg, gsWinner = Just winner, gsPlayedHistory = historialActualizado, gsFirstTileIndex = nuevoIndice }
                    Trancado resultado nuevoEstado ->
                        case vsMode of
                            TwoVsTwo ->
                                let jugadores = getJugadores nuevoEstado
                                    resultadoEquipos = determinarGanadorEquipos jugadores Nothing
                                    (_winningTeam, losingTeam, msg, winner) = case resultadoEquipos of
                                        GanadorEquipoDomino team _ -> 
                                            let teamName = teamToString team
                                                losing = if team == TeamA then TeamB else TeamA
                                            in (team, losing, "Gana " ++ teamName, Just $ T.pack teamName)
                                        GanadorEquipoTrancado team -> 
                                            let teamName = teamToString team
                                                losing = if team == TeamA then TeamB else TeamA
                                            in (team, losing, "Trancado. Gana " ++ teamName ++ " (menos puntos)", Just $ T.pack teamName)
                                        EmpateEquipos -> 
                                            (NoTeam, NoTeam, "Trancado. ¡Empate entre equipos!", Nothing)
                                    baseSession = session { gsState = nuevoEstado, gsSessionMessage = T.pack msg, gsWinner = winner, gsPlayedHistory = historialActualizado, gsFirstTileIndex = nuevoIndice }
                                    -- Actualizar Data a 100 si está activo (en empate no se suman puntos)
                                    finalSession = case (gsMatchState session, losingTeam) of
                                        (Just _, NoTeam) -> baseSession  -- Empate, no se actualizan puntos
                                        (Just _, losing) -> finalizeRound baseSession losing False
                                        (Nothing, _) -> baseSession
                                in finalSession
                            _ ->
                                let (msg, winner) = case resultado of
                                        GanadorDomino p -> 
                                            ("Trancado. Gana " ++ playerName p, Just $ T.pack $ playerName p)
                                        GanadorTrancado p -> 
                                            ("Trancado. Gana " ++ playerName p ++ " (menos puntos)", Just $ T.pack $ playerName p)
                                        Empate _ -> 
                                            ("Trancado. ¡Empate!", Nothing)
                                in session { gsState = nuevoEstado, gsSessionMessage = T.pack msg, gsWinner = winner, gsPlayedHistory = historialActualizado, gsFirstTileIndex = nuevoIndice }
                    ErrorAccion err -> 
                        session { gsSessionMessage = T.pack err }

-- | Convertir Team a String legible
teamToString :: Team -> String
teamToString TeamA = "Equipo A (Tú y Compañero)"
teamToString TeamB = "Equipo B (Rivales)"
teamToString NoTeam = "Sin equipo"

-- | Ejecutar pasar en modo NoRobadito (permite pasar aunque haya pozo)
ejecutarPasarNoRobadito :: GameSession -> GameState -> GameSession
ejecutarPasarNoRobadito session gs =
    let jugador = jugadorActual gs
        tablero = getTablero gs
        vsMode = gsVsMode session
    in if puedeJugar (playerHand jugador) tablero
         then session { gsSessionMessage = "No puedes pasar si tienes jugadas disponibles" }
         else
             let nuevosPases = gsPasesConsec gs + 1
                 nJugadores  = length (getJugadores gs)
                 gs1 = gs { gsPasesConsec = nuevosPases }
             in if nuevosPases >= nJugadores
                  then -- Todos pasaron, partida trancada
                    let gsFinal = gs1 { gsFase = Terminado }
                        jugadores = getJugadores gs
                        (msg, winner) = case vsMode of
                            TwoVsTwo ->
                                let resultadoEquipos = determinarGanadorEquipos jugadores Nothing
                                in case resultadoEquipos of
                                    GanadorEquipoDomino team _ -> 
                                        let teamName = teamToString team
                                        in ("Gana " ++ teamName, Just $ T.pack teamName)
                                    GanadorEquipoTrancado team -> 
                                        let teamName = teamToString team
                                        in ("Trancado. Gana " ++ teamName ++ " (menos puntos)", Just $ T.pack teamName)
                                    EmpateEquipos -> 
                                        ("Trancado. ¡Empate entre equipos!", Nothing)
                            _ ->
                                let resultado = determinarGanador jugadores Nothing
                                in case resultado of
                                    GanadorDomino p -> 
                                        ("Trancado. Gana " ++ playerName p, Just $ T.pack $ playerName p)
                                    GanadorTrancado p -> 
                                        ("Trancado. Gana " ++ playerName p ++ " (menos puntos)", Just $ T.pack $ playerName p)
                                    Empate _ -> 
                                        ("Trancado. ¡Empate!", Nothing)
                    in session { gsState = gsFinal, gsSessionMessage = T.pack msg, gsWinner = winner }
                  else 
                    let nuevoEstado = siguienteTurno gs1
                        msg = T.pack $ "Turno de " ++ playerName (jugadorActual nuevoEstado)
                    in session { gsState = nuevoEstado, gsSessionMessage = msg, gsWinner = Nothing }

-- | Parsear acción del JSON (considera el modo de juego)
parseAction :: GameAction -> GameMode -> Maybe Accion
parseAction (GameAction "play" (Just dj) (Just side)) _ =
    let domino = Domino (djLeft dj) (djRight dj)
        lado = if side == "left" then Izquierda else Derecha
    in Just $ Jugar domino lado
parseAction (GameAction "pass" _ _) _ = Just Pasar
parseAction (GameAction "draw" _ _) NoRobadito = Just Pasar  -- En NoRobadito, draw se convierte en pass
parseAction (GameAction "draw" _ _) Robadito = Just Robar
parseAction _ _ = Nothing

-- | Convertir estado del juego a JSON
gameStateToJson :: GameSession -> GameResponse
gameStateToJson session =
    let estado = gsState session
        tablero = getTablero estado
        jugadores = getJugadores estado
        turnoActual = gsTurnoActual estado
        jugadorAct = jugadores !! turnoActual
        terminado = estaTerminado estado
        pozo = getPozo estado
        canDrawBasedOnMode = case gsGameMode session of
            Robadito -> not (null pozo) && not terminado
            NoRobadito -> False
        gameModeText = case gsGameMode session of
            Robadito -> "robadito"
            NoRobadito -> "norobadito"
        vsModeText = case gsVsMode session of
            OneVsOne -> "1vs1"
            FreeForAll -> "freeforall"
            TwoVsTwo -> "2vs2"
        difficultyText = case gsDifficulty session of
            Easy -> "easy"
            Medium -> "medium"
            Hard -> "hard"
            Extreme -> "extreme"
        matchStateJson = fmap matchStateToJson (gsMatchState session)
        firstTileIdx = gsFirstTileIndex session
    in GameResponse
        { grBoard = boardToJson firstTileIdx tablero
        , grPlayers = map playerToJson jugadores
        , grCurrentPlayer = turnoActual
        , grCurrentPlayerName = T.pack $ playerName jugadorAct
        , grMessage = gsSessionMessage session
        , grGameOver = terminado
        , grWinner = gsWinner session
        , grPozoCount = length pozo
        , grCanDraw = canDrawBasedOnMode
        , grGameMode = gameModeText
        , grVsMode = vsModeText
        , grDifficulty = difficultyText
        , grMatchState = matchStateJson
        }

-- | Convertir MatchState a JSON
matchStateToJson :: MatchState -> MatchStateJson
matchStateToJson ms = MatchStateJson
    { msjTeamAScore = msTeamAScore ms
    , msjTeamBScore = msTeamBScore ms
    , msjRoundNumber = msRoundNumber ms
    , msjTargetScore = daTargetScore (msConfig ms)
    , msjMatchOver = msMatchOver ms
    , msjMatchWinner = fmap teamToText (msMatchWinner ms)
    , msjIsFirstRound = msRoundNumber ms == 1
    , msjBonusNoSalida = daBonusNoSalida (msConfig ms)
    , msjBonusPrimerDoble = daBonusPrimerDoble (msConfig ms)
    , msjBonusCapicua = daBonusCapicua (msConfig ms)
    }

-- | Convertir Board a BoardJson (usa el índice de la primera ficha para centrar)
boardToJson :: Int -> Board -> BoardJson
boardToJson firstTileIdx board = 
    let positions = reconstruirPosiciones firstTileIdx board
    in BoardJson
        { bjTiles = map visualTileToJson positions
        , bjLeftEnd = extremoIzquierdo board
        , bjRightEnd = extremoDerecho board
        }

-- | Convertir VisualTile a TileJson
visualTileToJson :: VisualTile -> TileJson
visualTileToJson vt = 
    let Domino l r = vtDomino vt
    in TileJson l r (vtX vt) (vtY vt) (vtIsVertical vt) (vtRotation vt)

playerToJson :: Player -> PlayerJson
playerToJson player = PlayerJson
    { pjName = T.pack $ playerName player
    , pjHand = map dominoToJson (playerHand player)
    , pjHandCount = length (playerHand player)
    , pjTeam = teamToText (playerTeam player)
    }

-- | Convertir Team a texto para JSON
teamToText :: Team -> T.Text
teamToText TeamA = "A"
teamToText TeamB = "B"
teamToText NoTeam = ""

dominoToJson :: Domino -> DominoJson
dominoToJson (Domino a b) = DominoJson a b

-- =========================================================
-- Posicionamiento Visual del Tablero
-- =========================================================

-- Dimensiones de las fichas en píxeles
tileWidth, tileHeight :: Int
tileWidth = 100   -- Ancho de ficha horizontal
tileHeight = 50   -- Alto de ficha horizontal (ancho de vertical)

-- Centro del tablero (punto de referencia) - FIJO, no cambia
boardCenterX, boardCenterY :: Int
boardCenterX = 550   -- Centro X del contenedor (1100 / 2)
boardCenterY = 350   -- Centro Y del contenedor (700 / 2)

-- Límites del tablero (para detectar cuando doblar)
boardMinX, boardMaxX :: Int
boardMinX = 50       -- Límite izquierdo
boardMaxX = 1050     -- Límite derecho (1100 - 50)

-- | Dirección de colocación de fichas
data Direccion = HaciaIzquierda | HaciaDerecha | HaciaAbajo | HaciaArriba
    deriving (Show, Eq)

-- | Estado de posicionamiento (para rastrear si hemos doblado)
data EstadoPosicion = EstadoPosicion
    { epX :: Int
    , epY :: Int
    , epDireccion :: Direccion
    , epRotacion :: Int       -- 0 o 180 para fichas horizontales después de doblar
    , epUltimaEsDoble :: Bool -- Si la última ficha colocada fue un doble
    } deriving (Show)

-- | Reconstruir todas las posiciones visuales desde el Board
-- firstTileIdx indica el índice de la primera ficha jugada (la que va en el centro)
reconstruirPosiciones :: Int -> Board -> BoardPositions
reconstruirPosiciones firstTileIdx board = 
    let fichas = fichasEnMesa board
        n = length fichas
    in case fichas of
        [] -> []
        [unica] -> [posicionarEnCentro unica]  -- Una sola ficha: centrada
        _ -> 
            let idx = min firstTileIdx (n - 1)
                fichasIzq = take idx fichas
                fichaCentral = fichas !! idx
                fichasDer = drop (idx + 1) fichas
                
                tileCentral = posicionarEnCentro fichaCentral
                fichaCentralEsDoble = case fichaCentral of Domino a b -> a == b
                
                -- Estado inicial para izquierda (empieza justo a la izquierda del centro)
                estadoIzqInicial = EstadoPosicion 
                    { epX = vtX tileCentral
                    , epY = boardCenterY
                    , epDireccion = HaciaIzquierda
                    , epRotacion = 0
                    , epUltimaEsDoble = fichaCentralEsDoble
                    }
                tilesIzq = posicionarConEsquinas (reverse fichasIzq) estadoIzqInicial True
                
                -- Estado inicial para derecha (empieza justo a la derecha del centro)
                estadoDerInicial = EstadoPosicion 
                    { epX = vtX tileCentral + anchoFicha fichaCentral + 3
                    , epY = boardCenterY
                    , epDireccion = HaciaDerecha
                    , epRotacion = 0
                    , epUltimaEsDoble = fichaCentralEsDoble
                    }
                tilesDer = posicionarConEsquinas fichasDer estadoDerInicial False
                
            in reverse tilesIzq ++ [tileCentral] ++ tilesDer

-- | Posicionar una ficha en el centro exacto del tablero
posicionarEnCentro :: Domino -> VisualTile
posicionarEnCentro d@(Domino a b) =
    let isDouble = a == b
        x = if isDouble 
            then boardCenterX - (tileHeight `div` 2)
            else boardCenterX - (tileWidth `div` 2)
        y = if isDouble
            then boardCenterY - (tileWidth `div` 2)
            else boardCenterY - (tileHeight `div` 2)
    in VisualTile d x y isDouble 0  -- Rotación 0

-- | Posicionar fichas con manejo de esquinas
-- esIzquierda: True = lado izquierdo del tablero (sube al doblar)
--              False = lado derecho del tablero (baja al doblar)
posicionarConEsquinas :: [Domino] -> EstadoPosicion -> Bool -> BoardPositions
posicionarConEsquinas [] _ _ = []
posicionarConEsquinas (d:ds) estado esIzquierda =
    let isDouble = case d of Domino _ _ -> let Domino a b = d in a == b
        ancho = anchoFicha d
        rotActual = epRotacion estado
        currentY = epY estado
        
        -- Determinar si llegamos al límite y necesitamos doblar
        (necesitaDoblar, newX) = case epDireccion estado of
            HaciaIzquierda -> 
                let nx = epX estado - ancho - 3
                in (nx < boardMinX, nx)
            HaciaDerecha -> 
                let nx = epX estado
                in (nx + ancho > boardMaxX, nx)
            _ -> (False, epX estado)
        
        (tile, nuevoEstado) = 
            if necesitaDoblar
            then -- DOBLAR: poner ficha vertical y cambiar dirección
                let -- Dirección vertical según el lado del tablero
                    haciaArriba = esIzquierda
                    
                    -- Si la última ficha fue un doble, ocupa más espacio vertical
                    ultimaEsDoble = epUltimaEsDoble estado
                    alturaUltima = if ultimaEsDoble then tileWidth else tileHeight
                    
                    -- Posición X de la ficha vertical
                    cornerX = case epDireccion estado of
                        HaciaIzquierda -> epX estado  -- Borde izquierdo de la última
                        HaciaDerecha   -> epX estado - 3 - tileHeight  -- A la izquierda del borde derecho
                        _              -> epX estado
                    
                    -- Posición Y de la ficha vertical (considerando si la última es doble)
                    cornerY = if haciaArriba
                        then currentY - (alturaUltima `div` 2) - tileWidth - 3  -- Arriba
                        else currentY + (alturaUltima `div` 2) + 3               -- Abajo
                    
                    t = VisualTile d cornerX cornerY True 0
                    
                    -- Nueva dirección horizontal (opuesta a la actual)
                    nuevaDirH = case epDireccion estado of
                        HaciaIzquierda -> HaciaDerecha
                        HaciaDerecha   -> HaciaIzquierda
                        _              -> HaciaDerecha
                    
                    -- Posición para la siguiente ficha (conecta al otro extremo de la vertical)
                    nextX = case nuevaDirH of
                        HaciaDerecha   -> cornerX + tileHeight + 3
                        HaciaIzquierda -> cornerX - 3
                        _              -> cornerX
                    
                    nextY = if haciaArriba
                        then cornerY + (tileHeight `div` 2)                     -- Centro de la fila superior
                        else cornerY + tileWidth - (tileHeight `div` 2)         -- Centro de la fila inferior
                    
                    nuevoEst = estado 
                        { epX = nextX
                        , epY = nextY
                        , epDireccion = nuevaDirH
                        , epRotacion = if rotActual == 0 then 180 else 0  -- Alternar rotación cada vez que doblamos
                        , epUltimaEsDoble = False  -- La ficha vertical no es doble visualmente
                        }
                in (t, nuevoEst)
            else -- CONTINUAR horizontal
                let y = if isDouble
                        then currentY - (tileWidth `div` 2)
                        else currentY - (tileHeight `div` 2)
                    t = VisualTile d newX y isDouble rotActual
                    
                    nextX = case epDireccion estado of
                        HaciaIzquierda -> newX
                        HaciaDerecha   -> newX + ancho + 3
                        _              -> newX
                    
                    nuevoEst = estado { epX = nextX, epUltimaEsDoble = isDouble }
                in (t, nuevoEst)
    
    in tile : posicionarConEsquinas ds nuevoEstado esIzquierda

-- | Calcular el ancho de una ficha según si es doble o no
anchoFicha :: Domino -> Int
anchoFicha (Domino a b) = if a == b then tileHeight else tileWidth

-- =========================================================
-- Data a 100 - Funciones de Puntuación
-- =========================================================

-- | Calcular puntos de la mano de un jugador
calcularPuntosMano :: Player -> Int
calcularPuntosMano p = sum [a + b | Domino a b <- playerHand p]

-- | Calcular puntos de un equipo
calcularPuntosEquipo :: [Player] -> Team -> Int
calcularPuntosEquipo jugadores team =
    sum [calcularPuntosMano p | p <- jugadores, playerTeam p == team]

-- | Verificar si la última jugada fue capicúa
esCapicua :: Board -> Bool
esCapicua board = case (extremoIzquierdo board, extremoDerecho board) of
    (Just izq, Just der) -> izq == der
    _ -> False

-- | Actualizar el estado de la partida (Data a 100) después de una ronda
updateMatchState :: GameSession -> Team -> Bool -> MatchState -> MatchState
updateMatchState session losingTeam wasCapicua ms =
    let jugadores = getJugadores (gsState session)
        cfg = msConfig ms
        basePoints = calcularPuntosEquipo jugadores losingTeam
        
        -- Bonus: No llevar salida (+20 puntos)
        -- Si el jugador inicial (turno 0) es del equipo perdedor y aún tiene fichas
        primerJugador = head jugadores
        noLlevoSalida = playerTeam primerJugador == losingTeam && not (null $ playerHand primerJugador)
        bonusNoSalida = if daBonusNoSalida cfg && noLlevoSalida then 20 else 0
        
        -- Bonus: Primer partido cuenta doble
        multiplicador = if daBonusPrimerDoble cfg && msRoundNumber ms == 1 then 2 else 1
        
        -- Bonus: Capicúa cuenta doble
        multiplicadorCapicua = if daBonusCapicua cfg && wasCapicua then 2 else 1
        
        -- Puntos totales de esta ronda
        puntosRonda = (basePoints + bonusNoSalida) * multiplicador * multiplicadorCapicua
        
        -- Actualizar puntuación (el equipo ganador recibe los puntos del perdedor)
        (newScoreA, newScoreB) = case losingTeam of
            TeamA -> (msTeamAScore ms, msTeamBScore ms + puntosRonda)  -- Si A pierde, B suma puntos
            TeamB -> (msTeamAScore ms + puntosRonda, msTeamBScore ms)  -- Si B pierde, A suma puntos
            NoTeam -> (msTeamAScore ms, msTeamBScore ms)
        
        -- Verificar si alguien llegó a 100
        targetScore = daTargetScore cfg
        matchOver = newScoreA >= targetScore || newScoreB >= targetScore
        matchWinner = if matchOver
            then if newScoreA >= targetScore then Just TeamA else Just TeamB  -- Quien llega a 100 gana
            else Nothing
        
        -- Actualizar quién ganó la primera ronda
        firstRoundWinner = if msRoundNumber ms == 1
            then Just (if losingTeam == TeamA then TeamB else TeamA)
            else msFirstRoundWinner ms
        
        -- Quién ganó esta ronda (el equipo que NO perdió)
        roundWinner = case losingTeam of
            TeamA -> Just TeamB
            TeamB -> Just TeamA
            NoTeam -> Nothing
    in ms
        { msTeamAScore = newScoreA
        , msTeamBScore = newScoreB
        , msRoundNumber = msRoundNumber ms + 1
        , msMatchOver = matchOver
        , msMatchWinner = matchWinner
        , msFirstRoundWinner = firstRoundWinner
        , msLastRoundCapicua = wasCapicua
        , msLastRoundNoSalida = if noLlevoSalida then Just losingTeam else Nothing
        , msLastRoundWinner = roundWinner
        }

-- | Iniciar la siguiente ronda en modo Data a 100
startNextRound :: GameSession -> IO GameSession
startNextRound session = case gsMatchState session of
    Nothing -> return session  -- No está en modo Data a 100
    Just ms | msMatchOver ms -> return session  -- La partida ya terminó
    Just ms -> do
        -- Crear nuevo estado de juego
        let nombres = ["Tú", "Rival 1", "Compañero", "Rival 2"]
            fichasPorJugador = 10
        nuevoEstado <- iniciarPartidaEquipos nombres fichasPorJugador
        -- Determinar quién empieza según el ganador de la ronda anterior
        -- Jugadores: 0=Tú (A), 1=Rival1 (B), 2=Compañero (A), 3=Rival2 (B)
        -- Si ganó TeamA, empieza jugador 0 (Tú)
        -- Si ganó TeamB, empieza jugador 1 (Rival 1)
        let turnoInicial = case msLastRoundWinner ms of
                Just TeamA -> 0  -- Tú
                Just TeamB -> 1  -- Rival 1
                _ -> 0           -- Por defecto Tú
            estadoConTurno = nuevoEstado { gsTurnoActual = turnoInicial }
            jugador = jugadorActual estadoConTurno
            ronda = msRoundNumber ms
            ganadorAnterior = case msLastRoundWinner ms of
                Just TeamA -> "Tu equipo"
                Just TeamB -> "Equipo rival"
                _ -> "Nadie"
        return session
            { gsState = estadoConTurno
            , gsSessionMessage = T.pack $ "Ronda " ++ show ronda ++ " - " ++ ganadorAnterior ++ " ganó la anterior. Turno de " ++ playerName jugador
            , gsWinner = Nothing
            , gsPlayedHistory = []
            , gsRoundWasCapicua = False
            , gsFirstTileIndex = 0  -- Reiniciar índice para nueva ronda
            }

-- | Finalizar ronda y actualizar puntuación de Data a 100
finalizeRound :: GameSession -> Team -> Bool -> GameSession
finalizeRound session losingTeam wasCapicua =
    case gsMatchState session of
        Nothing -> session
        Just ms ->
            let newMatchState = updateMatchState session losingTeam wasCapicua ms
                matchMsg = if msMatchOver newMatchState
                    then case msMatchWinner newMatchState of
                        Just TeamA -> "¡PARTIDA TERMINADA! Equipo A gana la Data a " ++ show (daTargetScore $ msConfig ms)
                        Just TeamB -> "¡PARTIDA TERMINADA! Equipo B gana la Data a " ++ show (daTargetScore $ msConfig ms)
                        _ -> "¡PARTIDA TERMINADA!"
                    else "Ronda terminada. " ++ 
                         "Equipo A: " ++ show (msTeamAScore newMatchState) ++ " pts | " ++
                         "Equipo B: " ++ show (msTeamBScore newMatchState) ++ " pts"
            in session 
                { gsMatchState = Just newMatchState
                , gsSessionMessage = T.pack matchMsg
                }