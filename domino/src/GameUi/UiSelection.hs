module GameUi.UiSelection
  ( UiSelection(..)
  , selectDomino
  , clearSelection
  , isSelected
  ) where

import Game.Domino (Domino)

-- | Represents the current selection state of the UI.
data UiSelection
    = NoSelection
    | Selected Domino
    deriving (Show, Eq)

-- | Selects a domino in the UI.
selectDomino :: Domino -> UiSelection
selectDomino = Selected

-- | Clears any current selection.
clearSelection :: UiSelection
clearSelection = NoSelection

-- | Checks whether a given domino is currently selected.
isSelected :: Domino -> UiSelection -> Bool
isSelected domino sel =
    case sel of
        Selected d -> d == domino
        NoSelection -> False