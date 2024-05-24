module Deathstacks where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Board

import Data.List (nub)

-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, steps :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ show startR ++ "-" ++ show tr ++ "-" ++ [tarC] ++ show tarR

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2 


-- #############################################################################
-- #################### playerWon :: Board -> Maybe Player #####################
-- #################### - 4 Functional Points              #####################
-- #################### - 1 Coverage Point                 #####################
-- #############################################################################

playerWon :: Board -> Maybe Player
playerWon board =
    let
        allStacks = concat board
        nonEmptyStacks = [p | Stack (p:_) <- allStacks]
    in
        case nonEmptyStacks of
            [] -> Nothing  -- No stacks, game cannot be won
            (firstPlayer:rest) ->
                if all (== firstPlayer) rest
                then Just firstPlayer
                else Nothing


-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- #############################################################################

possibleMoves :: Pos -> Cell -> [Move]
possibleMoves pos (Stack stack) = nub $ concatMap generateMovesForSteps [1..stackSize]
  where
    stackSize = length stack
    allDirections = [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest]
    generateMovesForSteps :: Int -> [Move]
    generateMovesForSteps steps = concatMap (generateMoves pos steps) allDirections
    generateMoves :: Pos -> Int -> Dir -> [Move]
    generateMoves startPos steps dir =
      let endPos = last (path startPos dir steps)
      in if endPos /= startPos then [Move startPos endPos steps] else []

-- Additional helper functions and implementations here

-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Functional Points                ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################



-- Checks if the move is valid according to the Death Stacks game rules
isValidMove :: Board -> Move -> Bool
isValidMove board (Move start target steps) =
    isPlayerTurnValid && isMovePossible && isTooTallRuleRespected
  where
    isPlayerTurnValid = case getStackAtPosition board start of
                         Just (Stack (player:_)) -> player == topPieceOfStartStack
                         _ -> False

    topPieceOfStartStack = case getStackAtPosition board start of
                              Just (Stack (p:_)) -> p
                              _ -> error "Invalid start position"  -- Shouldn't happen if board is valid

    isMovePossible = move `elem` (possibleMoves start (Stack (replicate steps topPieceOfStartStack)))


    isTooTallRuleRespected =
        let tooTallStacks = filter (isTooTall . snd) playerStacks
        in case tooTallStacks of
             [] -> True -- No too-tall stacks, any move is fine
             _  -> start `elem` map fst tooTallStacks && -- Move must start from a too-tall stack
                   remainingStackIsValid start steps

    remainingStackIsValid pos steps =
        case getStackAtPosition board pos of
            Just (Stack pieces) -> length pieces - steps <= 4
            _ -> False -- Should not happen if the board is valid


    playerStacks = [(pos, stack) | (pos, stack@(Stack (p:_))) <- getAllStacks board, p == topPieceOfStartStack]

    isTooTall (Stack pieces) = length pieces > 4
    isTooTall _ = False

    move = Move start target steps

-- Helper function to get the stack at a given position
getStackAtPosition :: Board -> Pos -> Maybe Cell
getStackAtPosition board pos =
    let (rowIndex, colIndex) = posToTuple pos
    in if rowIndex >= 0 && rowIndex < length board && colIndex >= 0 && colIndex < length (board !! rowIndex)
       then Just (board !! rowIndex !! colIndex)
       else Nothing

-- Helper function to get all stacks with their positions on the board
getAllStacks :: Board -> [(Pos, Cell)]
getAllStacks board = [(Pos (toEnum (colIndex + fromEnum 'a')) (rowIndex + 1), cell) 
                     | (rowIndex, rowCells) <- zip [0..] board
                     , (colIndex, cell) <- zip [0..] rowCells
                     , isStack cell]

-- Helper function to check if a cell contains a stack
isStack :: Cell -> Bool
isStack (Stack _) = True
isStack _ = False





-- #############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Functional Points                            ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################


listMoves :: Board -> Player -> [Move]
listMoves board player = concatMap generateMovesForStack playerStacks
  where
    playerStacks = [(pos, stack) | (pos, stack@(Stack (p:_))) <- getAllStacks board, p == player]

    generateMovesForStack :: (Pos, Cell) -> [Move]
    generateMovesForStack (pos, stack@(Stack pieces)) =
      filter (isValidMove board) $ possibleMoves pos stack

