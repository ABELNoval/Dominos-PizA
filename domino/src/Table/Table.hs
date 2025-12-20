module Table.Table (
    Table(..)
) where

import Player.Player (Player)
import Piece.Piece (Piece)

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
repartPieces pieces nplayers =
    let (hands, rest) = splitAt (Player * 10) pieces
        players =
          zipWith
            Player
            [1..nplayers]
            (chunksOf 10 hands)
    in (players, rest)

-- Auxiliar function to slash a list into a list of list
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
