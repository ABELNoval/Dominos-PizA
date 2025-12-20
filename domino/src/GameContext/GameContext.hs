module GameContext.GameContext(
    GameContext(..)
) where

import SystemState.SystemState (SystemState)
import World.World (World)

data GameContext = GameContext {
    world :: World,
    systemState :: SystemState
}