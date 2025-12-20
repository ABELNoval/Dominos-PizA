module Player
  ( Player(..)
  ) where

import Hand (Hand) 

data Player = Player
  { playerId   :: Int
  , playerName :: String
  , playerHand :: Hand
  } deriving (Show, Eq)
