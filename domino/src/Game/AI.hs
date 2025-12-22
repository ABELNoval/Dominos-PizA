module Game.AI
  ( chooseBotAction
  , Difficulty(..)
  ) where

import Game.GameState (GameState)
import Game.Actions (Accion(..))
import AI.Easy (chooseEasyMove)
import AI.Medium (chooseMediumMove)
import AI.Hard (chooseHardMove)

-- | Niveles de dificultad de la IA
data Difficulty = Easy | Medium | Hard
  deriving (Show, Eq)

-- | Elegir una acción para el bot según la dificultad seleccionada
chooseBotAction :: Difficulty -> GameState -> Accion
chooseBotAction difficulty gs = case difficulty of
  Easy   -> chooseEasyMove gs
  Medium -> chooseMediumMove gs
  Hard   -> chooseHardMove gs
