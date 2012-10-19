module Chessplayer.Helpers (readBoard, toString, pieceAt, switchColor, update, allMoves) 
       where

import Data.Char
import Data.List
import Chessplayer.Chesstypes

readBoard :: String -> Board
readBoard s = 
  (Board (map (map convertToPiece) $ (((extract s) \\ ["w"]) \\ ["b"])) color)
  where
    extract = reverse . filter (not . null) . lines . loseFormatting
    loseFormatting = filter (\x -> (x /= '|') && (x /= '-'))
    color
      | ((lines s) !! 10) !! 0 == 'w' = White
      | otherwise = Black
              
convertToPiece :: Char -> Piece
convertToPiece x
  | x == 'p' = (Piece Black Pawn)
  | x == 'n' = (Piece Black Knight)
  | x == 'b' = (Piece Black Bishop)
  | x == 'r' = (Piece Black Rook)
  | x == 'q' = (Piece Black Queen)
  | x == 'k' = (Piece Black King)
  | x == 'P' = (Piece White Pawn)
  | x == 'N' = (Piece White Knight)
  | x == 'B' = (Piece White Bishop)
  | x == 'R' = (Piece White Rook)
  | x == 'Q' = (Piece White Queen)
  | x == 'K' = (Piece White King)
  | x == ' ' = (Piece None Empty)
                    
toString :: Move -> String
toString (Move (Position sr sc) (Position er ec)) = 
  (['a'..'h']!!sc):intToDigit (sr + 1):'-':'>':(['a'..'h']!!ec):intToDigit(er + 1):[]

allMoves :: [Move]
allMoves = [(Move (Position sr sc) (Position er ec)) | sr <- [0..7], sc <- [0..7], er <- [0..7], ec <- [0..7]]

allPositions :: [[Position]]
allPositions = [[(Position r c) | c <- [0..7]] | r <- [0..7]]

pieceAt :: Position -> Board -> Piece
pieceAt (Position r c)  (Board b _) = (b !! r) !! c

switchColor :: Color -> Color
switchColor Black = White
switchColor White = Black
switchColor None = None

update :: Board -> Move -> Board
update brd@(Board b c) m@(Move (Position sr sc) (Position er ec)) = 
  (Board (map (map (updateHelper brd m)) allPositions) (switchColor c))
  
updateHelper :: Board -> Move -> Position -> Piece
updateHelper b (Move s e) p
  | s == p = (Piece None Empty)
  | e == p = pieceAt s b
  | otherwise = pieceAt p b