
module Board where  
import Data.List.Split (splitOn)
-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:


data Player = Red | Blue deriving Show
data Cell =  Stack [Player] | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
data Dir = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving Show --shikoje
type Board = [[Cell]]
instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)
instance Eq Player where
  (==) Blue Blue = True
  (==) Red Red = True
  (==) _ _ = False
instance Eq Cell where
  (==) Empty Empty = True
  (==) (Stack xs) (Stack ys) = xs == ys
  (==) _ _ = False

-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                  ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################


dirEquals :: Dir -> Dir -> Bool
dirEquals North North = True
dirEquals South South = True
dirEquals East East = True
dirEquals West West = True
dirEquals NorthEast NorthEast = True
dirEquals SouthWest SouthWest = True
dirEquals NorthWest NorthWest = True
dirEquals SouthEast SouthEast = True
dirEquals _ _ = False



isValidStack :: String -> Bool
isValidStack stack = all (`elem` ['r', 'b']) stack && not (null stack)
isValidCell :: String -> Bool
isValidCell cell = cell == "" || isValidStack cell
isValidRow :: String -> Bool
isValidRow row = let cells = splitOn "," row in length cells == 6 && all isValidCell cells
isValidBoard :: [String] -> Bool
isValidBoard board = length board == 6 && all isValidRow board
-- Main function to validate FEN
validateFEN :: String -> Bool
validateFEN fen = let rows = splitOn "/" fen in not (null fen) && isValidBoard rows




-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

-- Converts a character to a Player
charToPlayer :: Char -> Player
charToPlayer 'r' = Red
charToPlayer 'b' = Blue
charToPlayer _   = error "Invalid player character"
-- Converts a string representing a stack to a Cell
stringToCell :: String -> Cell
stringToCell "" = Empty
stringToCell stack = Stack (map charToPlayer stack)
-- Converts a string representing a row to a list of Cells
stringToRow :: String -> [Cell]
stringToRow row = map stringToCell (splitOn "," row)
-- Builds a Board from a valid FEN string
buildBoard :: String -> Board
buildBoard fen
    | validateFEN fen = map stringToRow (splitOn "/" fen)
    | otherwise = error "Invalid FEN string"
-- #############################################################################
-- #################### path :: Pos -> Dir -> Int -> [Pos]  ####################
-- #################### - 4 Functional Points               ####################
-- #################### - 1 Coverage Point                  ####################
-- #############################################################################

-- Converts a Pos and Dir to a numerical representation (row, col)
posToTuple :: Pos -> (Int, Int)
posToTuple (Pos col row) = (row - 1, fromEnum col - fromEnum 'a')
-- Converts a numerical representation (row, col) back to Pos
tupleToPos :: (Int, Int) -> Pos
tupleToPos (row, col) = Pos (toEnum (col + fromEnum 'a')) (row + 1)
-- Adjusts the direction based on board reflections
reflectDir :: Dir -> Dir
reflectDir North = South
reflectDir South = North
reflectDir East = West
reflectDir West = East
reflectDir NorthEast = SouthWest
reflectDir SouthWest = NorthEast
reflectDir NorthWest = SouthEast
reflectDir SouthEast = NorthWest
-- Moves one step in the given direction
move :: (Int, Int) -> Dir -> (Int, Int)
move (row, col) dir = case dir of
    North     -> (row + 1, col)
    South     -> (row - 1, col)
    East      -> (row, col + 1)
    West      -> (row, col - 1)
    NorthEast -> (row + 1, col + 1)
    SouthEast -> (row - 1, col + 1)
    NorthWest -> (row + 1, col - 1)
    SouthWest -> (row - 1, col - 1)
-- Checks if the position is within the board boundariess

-- moveAndReflect function updated to handle corners correctly
moveAndReflect :: ((Int, Int), Dir) -> ((Int, Int), Dir)
moveAndReflect ((row, col), currentDir) =
  let (newRow, newCol) = move (row, col) currentDir
      offBoardRow = newRow < 0 || newRow >= 6
      offBoardCol = newCol < 0 || newCol >= 6
      newDir = case (offBoardRow, offBoardCol) of
                 (True, True)   -> reflectDir currentDir
                 (True, False)  -> reflectDirRow currentDir
                 (False, True)  -> reflectDirCol currentDir
                 (False, False) -> currentDir
      correctedPos = move (row, col) newDir
  in (correctedPos, newDir)

-- Helper functions to reflect the direction on row and column
reflectDirRow :: Dir -> Dir
reflectDirRow North = South
reflectDirRow South = North
reflectDirRow NorthEast = SouthEast
reflectDirRow SouthEast = NorthEast
reflectDirRow NorthWest = SouthWest
reflectDirRow SouthWest = NorthWest
reflectDirRow other = other

reflectDirCol :: Dir -> Dir
reflectDirCol East = West
reflectDirCol West = East
reflectDirCol NorthEast = NorthWest
reflectDirCol SouthEast = SouthWest
reflectDirCol NorthWest = NorthEast
reflectDirCol SouthWest = SouthEast
reflectDirCol other = other

-- Main function to calculate path
path :: Pos -> Dir -> Int -> [Pos]
path startPos dir steps =
  let initialPos = posToTuple startPos
      pathWithDir = take (steps + 1) $ iterate moveAndReflect (initialPos, dir)
      pathPositions = map fst pathWithDir
  in map tupleToPos pathPositions