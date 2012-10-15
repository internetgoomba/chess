import Data.List
import Data.Function
import System.IO
import Data.Char
import System.Directory

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

main = do
  file <- readFile "./.chess.log"
  writeFile "./.chess2.log" $ toString . selectMove . readBoard $ file
  renameFile "./.chess2.log" "./.chess.log"

selectMove :: Board -> Move
selectMove b@(Board _ c) = 
  head . sortBy (compare `on` (utility . update b)) $ possibleMoves
  where
    possibleMoves = filter (not . inCheck c . update b) . filter (legal b) $ allMoves

utility :: Board -> Rational
utility b@(Board bd c) = -1 * (head . sort . map (naiveUtility . update b) $ possibleMoves)
  where
    possibleMoves = filter (not . inCheck c . update b) . filter (legal b) $ allMoves
    
naiveUtility :: Board -> Rational
naiveUtility b = foldr (+) 0 . map (naiveUtilityHelper b) . concat $ allPositions

naiveUtilityHelper :: Board -> Position -> Rational
naiveUtilityHelper b@(Board _ color) p@(Position r c) = 
  case pieceAt p b of
    (Piece White Pawn) -> ([0.8, 0.8, 0.9, 1, 1, 0.9, 0.8, 0.8] !! c) 
                          * ([0, 0, 0.25, 0.5, 1, 1, 1, 3] !! r) * cmod White
    (Piece Black Pawn) -> ([0.8, 0.8, 0.9, 1, 1, 0.9, 0.8, 0.8] !! c) 
                          * ([3, 1, 1, 1, 0.5, 0.25, 0, 0] !! r) * cmod Black
    (Piece n Knight) -> 3 * ([0.75, 0.8, 0.9, 1, 1, 0.9, 0.8, 0.85] !! c) * 
                        ([0.75, 0.8, 0.9, 1, 1, 0.9, 0.8, 0.75] !! r) * cmod n
    (Piece n Bishop) -> 3 * ([0.8, 0.9, 1, 1, 1, 1, 0.9, 0.8] !! c) 
                        * ([0.8, 0.9, 1, 1, 1, 1, 0.9, 0.8] !! r) * cmod n
    (Piece n Rook) -> 5 * ([0.8, 0.9, 1, 1, 1, 1, 0.9, 0.8] !! r) * cmod n
    (Piece n Queen) -> 9 * ([0.8, 0.9, 1, 1, 1, 1, 0.9, 0.8] !! c) 
                       * ([0.8, 0.9, 1, 1, 1, 1, 0.8, 0.9] !! r) * cmod n
    (Piece n King) -> 100 * cmod n
    (Piece _ Empty) -> 0
  where
    cmod = (\x -> if (x == color) then 1 else -1)

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

-- the various helper fns used
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

allMoves :: [Move]
allMoves = [(Move (Position sr sc) (Position er ec)) | sr <- [0..7], sc <- [0..7], er <- [0..7], ec <- [0..7]]

allPositions :: [[Position]]
allPositions = [[(Position r c) | c <- [0..7]] | r <- [0..7]]

pieceAt :: Position -> Board -> Piece
pieceAt (Position r c)  (Board b _) = (b !! r) !! c

color :: Piece -> Color
color (Piece c t) = c

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