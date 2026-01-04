module Game.AI
  ( chooseBotAction
  , chooseBotAction2v2
  , Difficulty(..)
  ) where

import Game.GameState (GameState)
import Game.Actions (Accion(..))
import AI.Easy (chooseEasyMove)
import AI.Medium (chooseMediumMove)
import AI.Hard (chooseHardMove)
import AI.Hard2v2 (chooseHard2v2Move, PlayedHistory)
import AI.Extreme (chooseExtremeMove)

-- | Niveles de dificultad de la IA
data Difficulty = Easy | Medium | Hard | Extreme
  deriving (Show, Eq)

-- | Elegir una acción para el bot según la dificultad seleccionada
chooseBotAction :: Difficulty -> GameState -> Accion
chooseBotAction difficulty gs = case difficulty of
  Easy    -> chooseEasyMove gs
  Medium  -> chooseMediumMove gs
  Hard    -> chooseHardMove gs
  Extreme -> chooseExtremeMove gs

-- | Elegir una acción para el bot en modo 2vs2 con historial
chooseBotAction2v2 :: Difficulty -> PlayedHistory -> GameState -> Accion
chooseBotAction2v2 difficulty history gs = case difficulty of
  Easy    -> chooseEasyMove gs
  Medium  -> chooseMediumMove gs
  Hard    -> chooseHard2v2Move history gs
  Extreme -> chooseExtremeMove gs  -- Extreme usa info perfecta, no necesita historial
