module World.World (
    World(..)
) where

import SystemState.SystemState (SystemState)
import Table.Table (Table)

data World = World {
    table :: Table,
    uiState :: SystemState
}