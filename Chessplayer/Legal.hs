module Chessplayer.Legal (legal, inCheck, possibleMoves)
       where

import Data.List
import Chessplayer.Chesstypes
import Chessplayer.Helpers
       
possibleMoves :: Board -> [Move]
possibleMoves b@(Board _ c) = filter (not. inCheck c . update b) . filter (legal b) $ allMoves

legal :: Board -> Move -> Bool
legal brd@(Board _ c) mv@(Move s@(Position sr sc) e@(Position er ec))
-- Piece must move
  | sr == er && sc == ec = False 
-- Must move own piece
  | (color $ pieceAt s brd) /= c = False
-- Can't take own piece
  | (color $ pieceAt e brd) == c = False
-- individual piece legality tests
  | (pieceAt s brd) == (Piece c King) = kingLegal brd mv
  | (pieceAt s brd) == (Piece c Queen) = queenLegal brd mv
  | (pieceAt s brd) == (Piece c Rook) = rookLegal brd mv
  | (pieceAt s brd) == (Piece c Knight) = knightLegal brd mv
  | (pieceAt s brd) == (Piece c Bishop) = bishopLegal brd mv
  | (pieceAt s brd) == (Piece c Pawn) = pawnLegal brd mv
  where
    color = (\(Piece col _) -> col)
                                                                     
kingLegal :: Board -> Move -> Bool
kingLegal _ (Move (Position sr sc) (Position er ec)) = 
  ((ec - sc) `elem` [-1, 0, 1]) && ((er - sr) `elem` [-1, 0, 1])

queenLegal :: Board -> Move -> Bool
queenLegal b m = rookLegal b m || bishopLegal b m

rookLegal :: Board -> Move -> Bool
rookLegal b m@(Move (Position sr sc) (Position er ec)) = 
  ((sr == er) || (sc == ec)) && not  (blocked b m)

bishopLegal :: Board -> Move -> Bool
bishopLegal b m@(Move (Position sr sc) (Position er ec)) =
  (((er - sr) == (ec - sc)) || ((er - sr) == (sc - ec))) && not (blocked b m)

knightLegal :: Board -> Move -> Bool
knightLegal b (Move (Position sr sc) (Position er ec)) = 
  (((er - sr) `elem` [-2, 2]) && ((ec - sc) `elem` [-1, 1])) ||
  (((er - sr) `elem` [-1, 1]) && ((ec - sc) `elem` [-2, 2]))

pawnLegal :: Board -> Move -> Bool
pawnLegal brd@(Board b c) m@(Move (Position sr sc) e@(Position er ec)) = 
  -- Pawns can move forward one step
  (((er - sr) == pawnDirection c) && 
   (ec == sc) && 
   (pieceAt e brd == (Piece None Empty))) ||
  -- must take diagonally forward
  (((er - sr) == pawnDirection c) && 
   (ec - sc) `elem` [-1, 1] && 
   (pieceAt e brd /= (Piece None Empty))) ||
  -- can double step of their start row
  (((er - sr) == 2 * (pawnDirection c)) &&
   (ec == sc) &&
   (pieceAt e brd == (Piece None Empty)) &&
   (sr == startRow c) &&
   (not (blocked brd m)))
  where
    -- pawns on each side go in different directions
    pawnDirection = (\x -> case x of
                        White -> 1
                        Black -> -1)
    startRow = (\x -> case x of
                        White -> 1
                        Black -> 6) 

blocked :: Board -> Move -> Bool
blocked b m = 
  foldl (||) False (map notEmpty [pieceAt (Position r c) b | (r, c) <- squaresCrossed m])
  where 
    notEmpty = (\(Piece _ t) -> t /= Empty)

squaresCrossed :: Move -> [(Int, Int)]
squaresCrossed (Move (Position sr sc) (Position er ec)) =
  zip rows cols
  where
    rows
      | (er == sr) = repeat sr
      | (sr < er ) = ([sr .. er] \\ [sr]) \\ [er]
      | (sr > er ) = reverse $ ([er .. sr] \\ [sr]) \\ [er]
    cols
      | (ec == sc) = repeat sc
      | (sc < ec ) = ([sc .. ec] \\ [sc]) \\ [ec]
      | (sc > ec ) = reverse $ ([ec .. sc] \\ [sc]) \\ [ec]

inCheck :: Color -> Board -> Bool
inCheck c b@(Board bd _) = 
  not . null . filter (kingAt) . filter (legal (Board bd (switchColor c))) $ allMoves
  where 
    kingAt = (\(Move _ e) -> pieceAt e b == (Piece c King))
