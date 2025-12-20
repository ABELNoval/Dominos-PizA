module Hand
  ( Hand(..)
  ) where

import Piece.Piece (Piece)

newtype Hand = Hand [Piece]
  deriving (Show, Eq)
