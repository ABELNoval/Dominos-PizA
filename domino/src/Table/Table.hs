module Table.Table (
    Table(..)
) where

import Hand.Hand (Hand(..))
import Piece.Piece (Piece(..))
import Player.Player (Player(..))
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

data Table = Table{
    players :: [Player],
    pieceInBoard :: [Piece]
}

-- Generate all pieces
generatePieces :: [Piece]
generatePieces =
  [Piece i j | i <- [0..9], j <- [i..9]]

-- Shuffle the generate pieces
shufflePieces :: [Piece] -> IO [Piece]
shufflePieces pieces = do
    shuffle' pieces (length pieces) <$> newStdGen

-- Repart the pieces for the number of players
repartPieces :: [Piece] -> Int -> ([Player], [Piece])
repartPieces pieces nPlayers =
    let (handsFlat, rest) = splitAt (nPlayers * 10) pieces
        hands = chunksOf 10 handsFlat
        playersList =
          [ Player playerIndex "" (Hand playerHand)
          | (playerIndex, playerHand) <- zip [1 .. nPlayers] hands
          ]
    in (playersList, rest)

-- Auxiliar function to slash a list into a list of list
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
