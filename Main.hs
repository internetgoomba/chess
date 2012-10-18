import System.IO
import System.Directory
import Data.Function
import Data.List

import Chessplayer.Chesstypes
import Chessplayer.Helpers
import Chessplayer.Legal
import Chessplayer.Utility

main = do
  file <- readFile "./.chess.log"
  writeFile "./.chess2.log" $ toString . selectMove . readBoard $ file
  renameFile "./.chess2.log" "./.chess.log"

selectMove :: Board -> Move
selectMove b@(Board _ c) = 
  maximumBy (compare `on` (utility b)) . possibleMoves $ b
