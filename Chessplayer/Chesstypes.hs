module Chessplayer.Chesstypes (Position (..), Move (..), Color (..), PieceType (..), Piece (..), Board (..))
       where

-- positions range from 0 0 to 7 7
data Position = Position Int Int deriving (Eq, Show)
data Move = Move Position Position deriving (Show)

-- so we can do color = c tests
data Color = White | Black | None deriving (Eq, Show)

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn | Empty 
               deriving (Eq, Show)
data Piece = Piece Color PieceType deriving (Eq, Show)

-- each row is a list of pieces, so board r c = (b !! r) !! c
-- color here = player color
data Board = Board [[Piece]] Color deriving (Show)

