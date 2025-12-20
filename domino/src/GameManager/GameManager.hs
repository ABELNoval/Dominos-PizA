module GameManager.GameManager
  (
  ) where

import GameContext.GameContext (GameContext)
import GameService.GameService (GameService)

-- Max Level:
-- Manage the current state of the app {gameContext} and the current services avaibles {gameServices}
data GameManager = GameManager{
    gameContext :: GameContext,
    gameService :: GameService
}
