module World.World (
    World(..)
) where

import Tabe.Table(Table)
import UiState.UiState(UiState)

data World = World {
    table :: Table,
    uiState :: UiState
}