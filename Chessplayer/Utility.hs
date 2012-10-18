module Chessplayer.Utility (utility) 
       where

import Data.List
import Data.Function
import Chessplayer.Chesstypes
import Chessplayer.Helpers
import Chessplayer.Legal

utility :: Board -> Move -> Double
utility = utility' 2

utility' :: Int -> Board -> Move -> Double
utility' 0 b m = naiveUtility b m
utility' n b@(Board _ c) m  
  | null . possibleMoves $ newb = 100
  | otherwise = naiveUtility b m - (maximum . map (utility' (n - 1) newb) . take 5 . reverse . sortBy (compare `on` naiveUtility newb) . possibleMoves $ newb)
  where
    newb = (update b m)

naiveUtility :: Board -> Move -> Double
naiveUtility b@(Board _ c) m@(Move s e) = 
   (naiveUtilityHelper newb c e) - (naiveUtilityHelper b c s) - (naiveUtilityHelper b c e)
  where 
    newb = (update b m)

naiveUtilityHelper :: Board -> Color -> Position -> Double
naiveUtilityHelper b col p@(Position r c) = 
  case pieceAt p b of
    (Piece White Pawn) -> ([1, 1, 1.1, 1.11, 1.12, 1.13, 1.14, 9] !! r) *
                          ([0, 0, 0.02, 0.03, 0.03, 0.02, 0, 0] !!c) * cmod White
    (Piece Black Pawn) -> ([9, 1.14, 1.13, 1.12, 1.11, 1.1, 1, 1] !! r) * 
                          ([0, 0, 0.02, 0.03, 0.03, 0.02, 0, 0] !!c) * cmod Black
    (Piece n Knight) -> 3 * (([0.95, 0.97, 0.99, 1, 1, 0.99, 0.97, 0.95] !! c) +
                        ([0.95, 0.97, 0.99, 1, 1, 0.99, 0.97, 0.95] !! r)) * cmod n
    (Piece n Bishop) -> 3 * ([0.97, 0.99, 1, 1, 1, 1, 0.99, 0.97] !! c) 
                        * ([0.97, 0.99, 1, 1, 1, 1, 0.99, 0.97] !! r) * cmod n
    (Piece n Rook) -> 5 * ([0.97, 0.99, 1, 1, 1, 1, 0.99, 0.97] !! r) 
                      * ([0.97, 0.97, 0.99, 1, 1, 0.99, 0.97, 0.97] !! c) * cmod n
    (Piece n Queen) -> 9 * ([0.97, 0.99, 1, 1, 1, 1, 0.99, 0.97] !! c) 
                       * ([0.97, 0.99, 1, 1, 1, 1, 0.99, 0.97] !! r) * cmod n
    (Piece n King) -> 100 * cmod n
    (Piece _ Empty) -> 0
  where
    cmod = (\x -> if (x == col) then 1 else -1)