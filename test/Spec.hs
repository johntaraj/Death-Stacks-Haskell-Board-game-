-- #############################################################################
-- ####### YOUR UNIT TESTS                                           ###########
-- ####### Note: execute tests using "stack test deathstacks:units"  ###########
-- #############################################################################

import Test.Hspec

import Board
import Deathstacks ( playerWon, isValidMove, listMoves, Move(Move), possibleMoves )


main :: IO ()
main = hspec $ do
    specy
    testValidateFEN
    testBuildBoard
    testPath
    testShowInstances
    testColAndRow
    testEqInstances
    testFalseValue
    testPlayerWon
    testPossibleMoves
    testIsValidMove
    testListMoves
    


specy :: Spec
specy = describe "IF Validate-Module-Board: reflectDir" $ do
    it "reflects North to South" $ do
        dirEquals (reflectDir North) South `shouldBe` True

    it "reflects South to North" $ do
        dirEquals (reflectDir South) North `shouldBe` True

    it "reflects East to West" $ do
        dirEquals (reflectDir East) West `shouldBe` True

    it "reflects West to East" $ do
        dirEquals (reflectDir West) East `shouldBe` True

    it "reflects NorthEast to SouthWest" $ do
        dirEquals (reflectDir NorthEast) SouthWest `shouldBe` True

    it "reflects SouthWest to NorthEast" $ do
        dirEquals (reflectDir SouthWest) NorthEast `shouldBe` True

    it "reflects NorthWest to SouthEast" $ do
        dirEquals (reflectDir NorthWest) SouthEast `shouldBe` True

    it "reflects SouthEast to NorthWest" $ do
        dirEquals (reflectDir SouthEast) NorthWest `shouldBe` True


sampleBoard :: Board
sampleBoard = [[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]

testValidateFEN :: Spec
testValidateFEN = describe "IF Validate-Module-Board: validateFEN ..." $ do
        it "empty string is not valid" $ do
            validateFEN "" `shouldBe` (False :: Bool)
        
        it "rejects FEN string with incorrect row count" $
            validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb/,,," `shouldBe` False

        it "rejects FEN string with incorrect cell count in a row" $
            validateFEN "rr,rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` False

        it "rejects FEN string with invalid characters in stack" $
            validateFEN "ra,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` False


        it "rejects FEN string with incomplete rows" $
            validateFEN "rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` False

        it "rejects FEN string with extra slashes" $
            validateFEN "rr,rr,rr,rr,rr,rr//,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` False

        it "rejects FEN string with incorrect cell formatting" $
            validateFEN "rr,rr;rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` False

        it "rejects FEN string starting or ending with delimiters" $
            validateFEN "/rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb/" `shouldBe` False

        it "rejects FEN string with empty rows" $
            validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,/,/bb,bb,bb,bb,bb,bb" `shouldBe` False

        it "rejects FEN string with mixed valid and invalid characters" $
            validateFEN "r2,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` False



testBuildBoard :: Spec
testBuildBoard = describe "IF Validate-Module-Board: buildBoard ..." $ do
        it "build empty board" $ do
            (buildBoard ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,") `shouldBe` ([[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]] :: Board)

-- extra tests
        it "builds a valid board from FEN string" $
            buildBoard "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe`
            [[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]


        it "builds a board with mixed stacks" $
            buildBoard "r,b,r,b,r,b/br,rb,br,rb,br,rb/,,,,,/,,,,,/,,,,,/b,r,b,r,b,r" `shouldBe`
                [[Stack [Red],Stack [Blue],Stack [Red],Stack [Blue],Stack [Red],Stack [Blue]],
                [Stack [Blue,Red],Stack [Red,Blue],Stack [Blue,Red],Stack [Red,Blue],Stack [Blue,Red],Stack [Red,Blue]],
                [Empty,Empty,Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty],
                [Stack [Blue],Stack [Red],Stack [Blue],Stack [Red],Stack [Blue],Stack [Red]]]

        it "builds a board with alternating stacks" $
            buildBoard "r,b,r,b,r,b/b,r,b,r,b,r/r,b,r,b,r,b/b,r,b,r,b,r/r,b,r,b,r,b/b,r,b,r,b,r" `shouldBe`
                [[Stack [Red],Stack [Blue],Stack [Red],Stack [Blue],Stack [Red],Stack [Blue]],
                [Stack [Blue],Stack [Red],Stack [Blue],Stack [Red],Stack [Blue],Stack [Red]],
                [Stack [Red],Stack [Blue],Stack [Red],Stack [Blue],Stack [Red],Stack [Blue]],
                [Stack [Blue],Stack [Red],Stack [Blue],Stack [Red],Stack [Blue],Stack [Red]],
                [Stack [Red],Stack [Blue],Stack [Red],Stack [Blue],Stack [Red],Stack [Blue]],
                [Stack [Blue],Stack [Red],Stack [Blue],Stack [Red],Stack [Blue],Stack [Red]]]

        -- Edge casess
        it "builds a board with maximum stack height" $
            buildBoard "rrrrrr,,,,,/,,,,,/,,,,,/,,,,,/,,,,,/bbbbbb,,,,," `shouldBe`
                [[Stack [Red,Red,Red,Red,Red,Red],Empty,Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty],
                [Stack [Blue,Blue,Blue,Blue,Blue,Blue],Empty,Empty,Empty,Empty,Empty]]

       
        it "builds a board with empty and full rows" $
            buildBoard "rrrrrr,,,,,/,,,,,/,,,,,/,,,,,/,,,,,/bbbbbb,,,,," `shouldBe`
                [[Stack [Red,Red,Red,Red,Red,Red],Empty,Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty],
                [Stack [Blue,Blue,Blue,Blue,Blue,Blue],Empty,Empty,Empty,Empty,Empty]]

        it "builds a board with alternating empty and full cells" $
            buildBoard "r,,r,,r,/,,b,,b,/r,,r,,r,/,,b,,b,/r,,r,,r,/,,b,,b," `shouldBe`
                [[Stack [Red],Empty,Stack [Red],Empty,Stack [Red],Empty],
                [Empty,Empty,Stack [Blue],Empty,Stack [Blue],Empty],
                [Stack [Red],Empty,Stack [Red],Empty,Stack [Red],Empty],
                [Empty,Empty,Stack [Blue],Empty,Stack [Blue],Empty],
                [Stack [Red],Empty,Stack [Red],Empty,Stack [Red],Empty],
                [Empty,Empty,Stack [Blue],Empty,Stack [Blue],Empty]]

        it "builds a board with single player cells" $
            buildBoard "r,r,r,r,r,r/,,,,,/,,,,,/,,,,,/,,,,,/b,b,b,b,b,b" `shouldBe`
                [[Stack [Red],Stack [Red],Stack [Red],Stack [Red],Stack [Red],Stack [Red]],
                [Empty,Empty,Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty],
                [Stack [Blue],Stack [Blue],Stack [Blue],Stack [Blue],Stack [Blue],Stack [Blue]]]

        it "builds a board with single cell stacks" $
            buildBoard "r,b,r,b,r,b/b,r,b,r,b,r/r,b,r,b,r,b/b,r,b,r,b,r/r,b,r,b,r,b/b,r,b,r,b,r" `shouldBe`
                [[Stack [Red],Stack [Blue],Stack [Red],Stack [Blue],Stack [Red],Stack [Blue]],
                [Stack [Blue],Stack [Red],Stack [Blue],Stack [Red],Stack [Blue],Stack [Red]],
                [Stack [Red],Stack [Blue],Stack [Red],Stack [Blue],Stack [Red],Stack [Blue]],
                [Stack [Blue],Stack [Red],Stack [Blue],Stack [Red],Stack [Blue],Stack [Red]],
                [Stack [Red],Stack [Blue],Stack [Red],Stack [Blue],Stack [Red],Stack [Blue]],
                [Stack [Blue],Stack [Red],Stack [Blue],Stack [Red],Stack [Blue],Stack [Red]]]

 -- Complete the expected board state


testPath :: Spec
testPath = describe "IF Validate-Module-Board: path ..." $ do


        -- Simple movements
        it "simple north - path (Pos 'c' 2) North 1" $ do
            path (Pos 'c' 2) North 1 `shouldBe` [Pos 'c' 2, Pos 'c' 3]

        it "simple northeast - path (Pos 'a' 1) NorthEast 5" $ do
            path (Pos 'a' 1) NorthEast 5 `shouldBe` [Pos 'a' 1, Pos 'b' 2, Pos 'c' 3, Pos 'd' 4, Pos 'e' 5, Pos 'f' 6]

        it "simple east - path (Pos 'a' 6) East 2" $ do
            path (Pos 'a' 6) East 2 `shouldBe` [Pos 'a' 6, Pos 'b' 6, Pos 'c' 6]

        it "simple southeast - path (Pos 'b' 3) SouthEast 2" $ do
            path (Pos 'b' 3) SouthEast 2 `shouldBe` [Pos 'b' 3, Pos 'c' 2, Pos 'd' 1]

        it "simple south - path (Pos 'd' 6) South 4" $ do
            path (Pos 'd' 6) South 4 `shouldBe` [Pos 'd' 6, Pos 'd' 5, Pos 'd' 4, Pos 'd' 3, Pos 'd' 2]

        it "simple southwest - path (Pos 'e' 2) SouthWest 1" $ do
            path (Pos 'e' 2) SouthWest 1 `shouldBe` [Pos 'e' 2, Pos 'd' 1]

        it "simple west - path (Pos 'f' 1) West 3" $ do
            path (Pos 'f' 1) West 3 `shouldBe` [Pos 'f' 1, Pos 'e' 1, Pos 'd' 1, Pos 'c' 1]

        it "simple northwest - path (Pos 'b' 5) NorthWest 1" $ do
            path (Pos 'b' 5) NorthWest 1 `shouldBe` [Pos 'b' 5, Pos 'a' 6]

        -- Mirror movements
        it "mirror north - path (Pos 'd' 5) North 2" $ do
            path (Pos 'd' 5) North 2 `shouldBe` [Pos 'd' 5, Pos 'd' 6, Pos 'd' 5]

        it "mirror south - path (Pos 'd' 5) South 3" $ do
            path (Pos 'd' 5) South 3 `shouldBe` [Pos 'd' 5, Pos 'd' 4, Pos 'd' 3, Pos 'd' 2]

        it "mirror east west bounce - path (Pos 'e' 5) East 8" $ do
            path (Pos 'e' 5) East 8 `shouldBe` [Pos 'e' 5, Pos 'f' 5, Pos 'e' 5, Pos 'd' 5, Pos 'c' 5, Pos 'b' 5, Pos 'a' 5, Pos 'b' 5, Pos 'c' 5]

        it "mirror diagonal 1 - path (Pos 'c' 5) NorthWest 8" $ do
            path (Pos 'c' 5) NorthWest 8 `shouldBe` [Pos 'c' 5, Pos 'b' 6, Pos 'a' 5, Pos 'b' 4, Pos 'c' 3, Pos 'd' 2, Pos 'e' 1, Pos 'f' 2, Pos 'e' 3]

        it "mirror diagonal 2 - path (Pos 'a' 2) NorthEast 6" $ do
            path (Pos 'a' 2) NorthEast 6 `shouldBe` [Pos 'a' 2, Pos 'b' 3, Pos 'c' 4, Pos 'd' 5, Pos 'e' 6, Pos 'f' 5, Pos 'e' 4]

        it "corner mirror - path (Pos 'f' 6) NorthEast 2" $ do
            path (Pos 'f' 6) NorthEast 2 `shouldBe` [Pos 'f' 6, Pos 'e' 5, Pos 'd' 4]

-- extra tests from me
        it "one step north" $ do
            path (Pos 'c' 2) North 1 `shouldBe` ([(Pos 'c' 2), (Pos 'c' 3)] :: [Pos])
        it "two steps east" $ do
            path (Pos 'b' 3) East 2 `shouldBe` [(Pos 'b' 3), (Pos 'c' 3), (Pos 'd' 3)]
        it "three steps south, with edge reflection" $ do
            path (Pos 'e' 2) South 3 `shouldBe` [(Pos 'e' 2), (Pos 'e' 1), (Pos 'e' 2), (Pos 'e' 3)]
        it "four steps west, with edge reflection" $ do
            path (Pos 'b' 4) West 4 `shouldBe` [(Pos 'b' 4), (Pos 'a' 4), (Pos 'b' 4), (Pos 'c' 4), (Pos 'd' 4)]
        it "no movement" $ do
            path (Pos 'd' 3) North 0 `shouldBe` [(Pos 'd' 3)]
        it "move south to edge" $ do
            path (Pos 'c' 3) South 2 `shouldBe` [(Pos 'c' 3), (Pos 'c' 2), (Pos 'c' 1)]
        it "multiple edge reflections, east and west" $ do
            path (Pos 'b' 1) East 10 `shouldBe` [(Pos 'b' 1), (Pos 'c' 1), (Pos 'd' 1), (Pos 'e' 1), (Pos 'f' 1), (Pos 'e' 1), (Pos 'd' 1), (Pos 'c' 1), (Pos 'b' 1), (Pos 'a' 1), (Pos 'b' 1)]
        it "north edge reflection" $ do
            path (Pos 'f' 5) North 2 `shouldBe` [(Pos 'f' 5), (Pos 'f' 6), (Pos 'f' 5)]
        -- New test cases for additional directions
        it "one step northeast" $ do
            path (Pos 'c' 2) NorthEast 1 `shouldBe` [(Pos 'c' 2), (Pos 'd' 3)]
        it "two steps southeast" $ do
            path (Pos 'b' 4) SouthEast 2 `shouldBe` [(Pos 'b' 4), (Pos 'c' 3), (Pos 'd' 2)]

        it "four steps northwest, with edge reflection" $ do
            path (Pos 'd' 3) NorthWest 4 `shouldBe` [(Pos 'd' 3), (Pos 'c' 4), (Pos 'b' 5), (Pos 'a' 6), (Pos 'b' 5)]
        --testet e gitlab which failed    
        -- Test for the first failure case
        it "eight steps northwest from 'c5', with multiple edge reflections" $ do
            path (Pos 'c' 5) NorthWest 8 `shouldBe` [Pos {col = 'c', row = 5}, Pos {col = 'b', row = 6}, Pos {col = 'a', row = 5}, Pos {col = 'b', row = 4}, Pos {col = 'c', row = 3}, Pos {col = 'd', row = 2}, Pos {col = 'e', row = 1}, Pos {col = 'f', row = 2}, Pos {col = 'e', row = 3}]
        -- Test for the second failure case
        it "six steps northeast from 'a2', with edge reflection" $ do
            path (Pos 'a' 2) NorthEast 6 `shouldBe` [Pos {col = 'a', row = 2}, Pos {col = 'b', row = 3}, Pos {col = 'c', row = 4}, Pos {col = 'd', row = 5}, Pos {col = 'e', row = 6}, Pos {col = 'f', row = 5}, Pos {col = 'e', row = 4}]
        
        it "complex path with multiple reflections" $ do
            path (Pos 'd' 1) South 7 `shouldBe` [(Pos 'd' 1), (Pos 'd' 2), (Pos 'd' 3), (Pos 'd' 4), (Pos 'd' 5), (Pos 'd' 6), (Pos 'd' 5), (Pos 'd' 4)]
        
        -- extra se possiblemoves is failing
        it "  d4-4-d2 south" $ do
            path (Pos 'd' 4) South 4 `shouldBe` [(Pos 'd' 4), (Pos 'd' 3), (Pos 'd' 2), (Pos 'd' 1), (Pos 'd' 2) ]
        
        it "d4-4-b4 west " $ do
            path (Pos 'd' 4) West 4 `shouldBe` [(Pos 'd' 4), (Pos 'c' 4), (Pos 'b' 4), (Pos 'a' 4), (Pos 'b' 4) ]
            


testShowInstances :: Spec
testShowInstances = describe "IF Validate-Module-Board: Show instances ..." $ do
    it "shows Red player correctly" $ do
        show Red `shouldBe` "Red"
    -- Add more tests for other Show instances here...

testColAndRow :: Spec
testColAndRow = describe "IF Validate-Module-Board: col and row ..." $ do
    it "gets column correctly" $ do
        col (Pos 'a' 1) `shouldBe` 'a'
    -- Add more tests for col and row here...

testEqInstances :: Spec
testEqInstances = describe "IF Validate-Module-Board: Eq instances ..." $ do
    it "compares Pos correctly" $ do
        (Pos 'a' 1) == (Pos 'a' 1) `shouldBe` True
    -- Add more tests for other Eq instances here...

testFalseValue :: Spec
testFalseValue = describe "IF Validate-Module-Board: False value ..." $ do
    it "compares Pos correctly" $ do
        (Pos 'a' 1) == (Pos 'a' 2) `shouldBe` False
    -- Add more tests for other Eq instances here...

---------------------------------------------------------------------




testPlayerWon :: Spec
testPlayerWon = describe "IF Validate-Module-Game: playerWon ..." $ do
    it "start board not finished" $ do
        let board = buildBoard "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb"
        playerWon board `shouldBe` Nothing

    it "red wins - all stacks red on top" $ do
        let board = buildBoard "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,"
        playerWon board `shouldBe` Just Red

    it "blue wins - all stacks blue on top" $ do
        let board = buildBoard ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb"
        playerWon board `shouldBe` Just Blue

    it "mixed stacks - game not finished" $ do
        let board = buildBoard "rr,bb,rr,,,/bb,rr,bb,,,/,,,,,/,,,,,/,,,,,/,,,,,"
        playerWon board `shouldBe` Nothing

    it "empty board - game not finished" $ do
        let board = buildBoard ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,"
        playerWon board `shouldBe` Nothing

    it "mixed stacks - game not finished" $ do
        let board = buildBoard "rr,bb,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb"
        playerWon board `shouldBe` Nothing

    it "red wins with mixed stack heights" $ do
        let board = buildBoard "r,r,r,r,r,r/r,r,r,r,r,/,,,,,/,,,,,/,,,,,/,,,,,"
        playerWon board `shouldBe` Just Red

    
    it "blue wins with complex stacks" $ do
        let board = buildBoard "bbr,bb,bb,bb,bb,bb/br,br,br,br,br,br/,,,,,/,,,,,/,,,,,/,,,,,"
        playerWon board `shouldBe` Just Blue

    it "stalemate - no moves left" $ do
        let board = buildBoard "rr,bb,rr,bb,rr,bb/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,"
        playerWon board `shouldBe` Nothing

    it "game continues after complex moves" $ do
        let board = buildBoard "r,brb,rbb,r,r,rb/b,r,br,b,r,br/,,,,,/,,,,,/,,,,,/,,,,,"
        playerWon board `shouldBe` Nothing

--extra extraaaa

    it "Only one stack on board - Red wins" $ do
        let board = buildBoard ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,r,"
        playerWon board `shouldBe` Just Red
    
   
    it "Alternating top pieces on each stack - Game not finished" $ do
        let board = buildBoard "rb,br,rb,br,rb,br/br,rb,br,rb,br,rb/rb,br,rb,br,rb,br/br,rb,br,rb,br,rb/rb,br,rb,br,rb,br/br,rb,br,rb,br,rb"
        playerWon board `shouldBe` Nothing

    it "All cells empty - Game not finished" $ do
        let board = buildBoard ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,"
        playerWon board `shouldBe` Nothing

    it "Complex stacks with no clear winner - Game not finished" $ do
        let board = buildBoard "rrbrb,bbrrb,rbrbb,brbrb,rbbrr,bbrrb/brbrb,rbrrb,brbrb,rbrbr,bbrrb,brbrb/rrbrb,brbrb,rrbrb,brbrb,rbrbb,bbrrb/rbrbb,brbrb,rrbrb,bbrrb,rbrbb,brbrb/brbrb,rrbrb,brbrb,rbrbr,bbrrb,brbrb/rbrbb,brbrb,rrbrb,bbrrb,rrbrb,bbrrb"
        playerWon board `shouldBe` Nothing

testPossibleMoves :: Spec
testPossibleMoves = describe "IF Validate-Module-Game: possibleMoves ..." $ do
        it "single test" $ do
            possibleMoves (Pos 'b' 1) (Stack [Red]) `shouldMatchList` ([Move (Pos 'b' 1) (Pos 'b' 2) 1,Move (Pos 'b' 1) (Pos 'c' 2) 1,Move (Pos 'b' 1) (Pos 'c' 1) 1,Move (Pos 'b' 1) (Pos 'a' 2) 1,Move (Pos 'b' 1) (Pos 'a' 1) 1] :: [Move])

        it "stack of 2 in b-1" $ do
            possibleMoves (Pos 'b' 1) (Stack [Red,Red]) `shouldMatchList` ([Move (Pos 'b' 1) (Pos 'b' 2) 1,Move (Pos 'b' 1) (Pos 'c' 2) 1,Move (Pos 'b' 1) (Pos 'c' 1) 1,Move (Pos 'b' 1) (Pos 'a' 2) 1,Move (Pos 'b' 1) (Pos 'a' 1) 1, Move (Pos 'b' 1) (Pos 'b' 3) 2,Move (Pos 'b' 1) (Pos 'd' 3) 2,Move (Pos 'b' 1) (Pos 'd' 1) 2 ] :: [Move])
        
        it "stack of 3 in b-1" $ do
            possibleMoves (Pos 'b' 1) (Stack [Red,Red,Red]) `shouldMatchList` ([Move (Pos 'b' 1) (Pos 'b' 2) 1,Move (Pos 'b' 1) (Pos 'c' 2) 1,Move (Pos 'b' 1) (Pos 'c' 1) 1,Move (Pos 'b' 1) (Pos 'a' 2) 1,Move (Pos 'b' 1) (Pos 'a' 1) 1, Move (Pos 'b' 1) (Pos 'b' 3) 2,Move (Pos 'b' 1) (Pos 'd' 3) 2, Move (Pos 'b' 1) (Pos 'd' 1) 2, Move (Pos 'b' 1) (Pos 'b' 4) 3,Move (Pos 'b' 1) (Pos 'e' 4) 3,Move (Pos 'b' 1) (Pos 'c' 1) 3,Move (Pos 'b' 1) (Pos 'e' 1) 3, Move (Pos 'b' 1) (Pos 'c' 4) 3] :: [Move])

        it "stack of 4 in b-1" $ do
            possibleMoves (Pos 'b' 1) (Stack [Red,Red,Red,Red]) `shouldMatchList` ([Move (Pos 'b' 1) (Pos 'b' 2) 1,Move (Pos 'b' 1) (Pos 'c' 2) 1,Move (Pos 'b' 1) (Pos 'c' 1) 1,Move (Pos 'b' 1) (Pos 'a' 2) 1,Move (Pos 'b' 1) (Pos 'a' 1) 1, Move (Pos 'b' 1) (Pos 'b' 3) 2,Move (Pos 'b' 1) (Pos 'd' 3) 2, Move (Pos 'b' 1) (Pos 'd' 1) 2, Move (Pos 'b' 1) (Pos 'b' 4) 3,Move (Pos 'b' 1) (Pos 'e' 4) 3,Move (Pos 'b' 1) (Pos 'c' 1) 3,Move (Pos 'b' 1) (Pos 'e' 1) 3, Move (Pos 'b' 1) (Pos 'c' 4) 3, Move (Pos 'b' 1) (Pos 'b' 5) 4,Move (Pos 'b' 1) (Pos 'f' 5) 4,Move (Pos 'b' 1) (Pos 'd' 1) 4,Move (Pos 'b' 1) (Pos 'f' 1) 4,Move (Pos 'b' 1) (Pos 'd' 5) 4] :: [Move])

        it "stack of 1 in a-1 easy" $ do
            possibleMoves (Pos 'a' 1) (Stack [Red]) `shouldMatchList` ([Move (Pos 'a' 1) (Pos 'a' 2) 1,Move (Pos 'a' 1) (Pos 'b' 2) 1,Move (Pos 'a' 1) (Pos 'b' 1) 1] :: [Move])
        
        it "stack of 2 in a-1 easy" $ do
            possibleMoves (Pos 'a' 1) (Stack [Red,Red]) `shouldMatchList` ([Move (Pos 'a' 1) (Pos 'a' 2) 1,Move (Pos 'a' 1) (Pos 'b' 2) 1,Move (Pos 'a' 1) (Pos 'b' 1) 1, Move (Pos 'a' 1) (Pos 'a' 3) 2,Move (Pos 'a' 1) (Pos 'c' 1 ) 2,Move (Pos 'a' 1) (Pos 'c' 3) 2] :: [Move])

        it "stack of 3 in a-1 easy" $ do
            possibleMoves (Pos 'a' 1) (Stack [Red,Red,Red]) `shouldMatchList` ([Move (Pos 'a' 1) (Pos 'a' 2) 1,Move (Pos 'a' 1) (Pos 'b' 2) 1,Move (Pos 'a' 1) (Pos 'b' 1) 1, Move (Pos 'a' 1) (Pos 'a' 3) 2,Move (Pos 'a' 1) (Pos 'c' 1 ) 2,Move (Pos 'a' 1) (Pos 'c' 3) 2, Move (Pos 'a' 1) (Pos 'a' 4) 3,Move (Pos 'a' 1) (Pos 'd' 1 ) 3,Move (Pos 'a' 1) (Pos 'd' 4) 3] :: [Move])
       
        it "stack of 1 in d-4" $ do
            possibleMoves (Pos 'd' 4) (Stack [Red]) `shouldMatchList` ([Move (Pos 'd' 4) (Pos 'd' 5) 1,Move (Pos 'd' 4) (Pos 'e' 5) 1,Move (Pos 'd' 4) (Pos 'e' 4) 1,Move (Pos 'd' 4) (Pos 'c' 5) 1,Move (Pos 'd' 4) (Pos 'c' 4) 1,Move (Pos 'd' 4) (Pos 'c' 3) 1,Move (Pos 'd' 4) (Pos 'd' 3) 1,Move (Pos 'd' 4) (Pos 'e' 3) 1] :: [Move])
        
        it "stack of 2 in d-4" $ do
            possibleMoves (Pos 'd' 4) (Stack [Red,Red]) `shouldMatchList` ([Move (Pos 'd' 4) (Pos 'd' 5) 1,Move (Pos 'd' 4) (Pos 'e' 5) 1,Move (Pos 'd' 4) (Pos 'e' 4) 1,Move (Pos 'd' 4) (Pos 'c' 5) 1,Move (Pos 'd' 4) (Pos 'c' 4) 1,Move (Pos 'd' 4) (Pos 'c' 3) 1,Move (Pos 'd' 4) (Pos 'd' 3) 1,Move (Pos 'd' 4) (Pos 'e' 3) 1, Move (Pos 'd' 4) (Pos 'd' 6) 2,Move (Pos 'd' 4) (Pos 'f' 6) 2,Move (Pos 'd' 4) (Pos 'f' 4) 2,Move (Pos 'd' 4) (Pos 'b' 6) 2,Move (Pos 'd' 4) (Pos 'b' 4) 2,Move (Pos 'd' 4) (Pos 'b' 2) 2,Move (Pos 'd' 4) (Pos 'd' 2) 2,Move (Pos 'd' 4) (Pos 'f' 2) 2] :: [Move])
        

        it "stack of 3 in d-4" $ do
            possibleMoves (Pos 'd' 4) (Stack [Red,Red,Red]) `shouldMatchList` ([Move (Pos 'd' 4) (Pos 'd' 5) 1,Move (Pos 'd' 4) (Pos 'e' 5) 1,Move (Pos 'd' 4) (Pos 'e' 4) 1,Move (Pos 'd' 4) (Pos 'c' 5) 1,Move (Pos 'd' 4) (Pos 'c' 4) 1,Move (Pos 'd' 4) (Pos 'c' 3) 1,Move (Pos 'd' 4) (Pos 'd' 3) 1,Move (Pos 'd' 4) (Pos 'e' 3) 1,    Move (Pos 'd' 4) (Pos 'd' 6) 2,Move (Pos 'd' 4) (Pos 'f' 6) 2,Move (Pos 'd' 4) (Pos 'f' 4) 2,Move (Pos 'd' 4) (Pos 'b' 6) 2,Move (Pos 'd' 4) (Pos 'b' 4) 2,Move (Pos 'd' 4) (Pos 'b' 2) 2,Move (Pos 'd' 4) (Pos 'd' 2) 2,Move (Pos 'd' 4) (Pos 'f' 2) 2,   Move (Pos 'd' 4) (Pos 'd' 5) 3,Move (Pos 'd' 4) (Pos 'e' 5) 3,Move (Pos 'd' 4) (Pos 'e' 4) 3,Move (Pos 'd' 4) (Pos 'a' 5) 3,Move (Pos 'd' 4) (Pos 'a' 4) 3,Move (Pos 'd' 4) (Pos 'a' 1) 3,Move (Pos 'd' 4) (Pos 'd' 1) 3,Move (Pos 'd' 4) (Pos 'e' 1) 3] :: [Move])


        it "stack of 4 in d-4" $ do
            possibleMoves (Pos 'd' 4) (Stack [Red,Red,Red, Blue]) `shouldMatchList` ([Move (Pos 'd' 4) (Pos 'd' 5) 1,Move (Pos 'd' 4) (Pos 'e' 5) 1,Move (Pos 'd' 4) (Pos 'e' 4) 1,Move (Pos 'd' 4) (Pos 'c' 5) 1,Move (Pos 'd' 4) (Pos 'c' 4) 1,Move (Pos 'd' 4) (Pos 'c' 3) 1,Move (Pos 'd' 4) (Pos 'd' 3) 1,Move (Pos 'd' 4) (Pos 'e' 3) 1,    Move (Pos 'd' 4) (Pos 'd' 6) 2,Move (Pos 'd' 4) (Pos 'f' 6) 2,Move (Pos 'd' 4) (Pos 'f' 4) 2,Move (Pos 'd' 4) (Pos 'b' 6) 2,Move (Pos 'd' 4) (Pos 'b' 4) 2,Move (Pos 'd' 4) (Pos 'b' 2) 2,Move (Pos 'd' 4) (Pos 'd' 2) 2,Move (Pos 'd' 4) (Pos 'f' 2) 2,   Move (Pos 'd' 4) (Pos 'd' 5) 3,Move (Pos 'd' 4) (Pos 'e' 5) 3,Move (Pos 'd' 4) (Pos 'e' 4) 3,Move (Pos 'd' 4) (Pos 'a' 5) 3,Move (Pos 'd' 4) (Pos 'a' 4) 3,Move (Pos 'd' 4) (Pos 'a' 1) 3,Move (Pos 'd' 4) (Pos 'd' 1) 3,Move (Pos 'd' 4) (Pos 'e' 1) 3,Move (Pos 'd' 4) (Pos 'b' 4) 4, Move (Pos 'd' 4) (Pos 'b' 2) 4,Move (Pos 'd' 4) (Pos 'd' 2) 4 ] :: [Move])  
        
        

        it "kot sa per commit -stack of 4 in d-4" $ do
            possibleMoves (Pos 'd' 4) (Stack [Red,Red,Red, Blue]) `shouldMatchList` ([Move (Pos 'd' 4) (Pos 'd' 5) 1,Move (Pos 'd' 4) (Pos 'e' 5) 1,Move (Pos 'd' 4) (Pos 'e' 4) 1,Move (Pos 'd' 4) (Pos 'c' 5) 1,Move (Pos 'd' 4) (Pos 'c' 4) 1,Move (Pos 'd' 4) (Pos 'c' 3) 1,Move (Pos 'd' 4) (Pos 'd' 3) 1,Move (Pos 'd' 4) (Pos 'e' 3) 1,    Move (Pos 'd' 4) (Pos 'd' 6) 2,Move (Pos 'd' 4) (Pos 'f' 6) 2,Move (Pos 'd' 4) (Pos 'f' 4) 2,Move (Pos 'd' 4) (Pos 'b' 6) 2,Move (Pos 'd' 4) (Pos 'b' 4) 2,Move (Pos 'd' 4) (Pos 'b' 2) 2,Move (Pos 'd' 4) (Pos 'd' 2) 2,Move (Pos 'd' 4) (Pos 'f' 2) 2,   Move (Pos 'd' 4) (Pos 'd' 5) 3,Move (Pos 'd' 4) (Pos 'e' 5) 3,Move (Pos 'd' 4) (Pos 'e' 4) 3,Move (Pos 'd' 4) (Pos 'a' 5) 3,Move (Pos 'd' 4) (Pos 'a' 4) 3,Move (Pos 'd' 4) (Pos 'a' 1) 3,Move (Pos 'd' 4) (Pos 'd' 1) 3,Move (Pos 'd' 4) (Pos 'e' 1) 3,Move (Pos 'd' 4) (Pos 'b' 4) 4, Move (Pos 'd' 4) (Pos 'b' 2) 4,Move (Pos 'd' 4) (Pos 'd' 2) 4 ] :: [Move])  
        

testIsValidMove :: Spec
testIsValidMove = describe "IF Validate-Module-Game: isValidMove ..." $ do
        it "simple valid move" $ do
            let b = [[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
                in isValidMove b (Move (Pos 'a' 6) (Pos 'a' 4) 2) `shouldBe` (True :: Bool)

        it "wrong step calculation" $ do
            let b = [[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
                in isValidMove b (Move (Pos 'a' 6) (Pos 'a' 4) 1) `shouldBe` (False :: Bool) --duhet 2 jo 1
        

        it "valid move: single piece one space" $ do
             let b = [[Stack [Red,Red,Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
                in isValidMove b (Move (Pos 'a' 6) (Pos 'a' 5) 1) `shouldBe` (True :: Bool) -- leviz nje kuti true


        it "to tall ignored move red " $ do
            let b = [[Stack [Red,Red,Red,Red, Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
                in isValidMove b (Move (Pos 'b' 1) (Pos 'b' 3) 2) `shouldBe` (False :: Bool)   --rregulli too tall duhet te jete prioritet por seshte , merret tjetr stack instead.
        
        
        it "to tall is valid move red" $ do
            let b = [[Stack [Red,Red,Red,Red, Red, Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
                in isValidMove b (Move (Pos 'a' 1) (Pos 'a' 3) 2) `shouldBe` (True :: Bool)-- duhet te mbetet stacku me max 4 elemente
        

        it " to-tall respected but not enough moved red" $ do
            let b = [[Stack [Red,Red,Red,Red,Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
                in isValidMove b (Move (Pos 'a' 1) (Pos 'a' 2) 1) `shouldBe` (False :: Bool) -- stacku ka 6 beht me 5, duhet 4 aber deswegen falsch


        
        it "to tall ignored coz is red but we are in the turn of blue " $ do
            let b = [[Stack [Red,Red,Red,Red, Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
                in isValidMove b (Move (Pos 'a' 6) (Pos 'a' 4) 2) `shouldBe` (True :: Bool)   --rregulli too tall duhet te jete prioritet por seshte , merret tjetr stack instead.
       --bluee
          
        it "valid move blue" $ do
            let b = [[Stack [Red,Red,Red,Red, Red, Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
                in isValidMove b (Move (Pos 'a' 6) (Pos 'a' 4) 2) `shouldBe` (True :: Bool)-- duhet te mbetet stacku me max 4 elemente
         
        it "FAIL move blue straigth" $ do
            let b = [[Stack [Red,Red,Red,Red, Red, Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
                in isValidMove b (Move (Pos 'a' 6) (Pos 'a' 3) 2) `shouldBe` (False :: Bool)-- duhet te mbetet stacku me max 4 elemente
        
        it "FAIL move blue but diagonally" $ do
            let b = [[Stack [Red,Red,Red,Red, Red, Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
                in isValidMove b (Move (Pos 'a' 6) (Pos 'c' 3) 2) `shouldBe` (False :: Bool)--
         
        it "TRUE move blue but diagonally" $ do
            let b = [[Stack [Red,Red,Red,Red, Red, Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]
                in isValidMove b (Move (Pos 'a' 6) (Pos 'd' 3) 3) `shouldBe` (True :: Bool)--
        

        it "FAIL TOOO TALL  blue we move not a tootall before the tootall" $ do
            let b = [[Stack [Red,Red,Red,Red, Red, Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue, Blue, Blue, Blue]]]
                in isValidMove b (Move (Pos 'a' 6) (Pos 'a' 4) 2) `shouldBe` (False :: Bool)-- the tootall is in f6, should be moved first
        
        
        it "FAIL TOOO TALL  blue we move not a tootall before the tootall" $ do
            let b = [[Stack [Red,Red,Red,Red, Red, Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue, Blue, Blue,Blue],Stack [Blue, Blue,Blue, Blue, Blue, Blue]]]
                in isValidMove b (Move (Pos 'e' 6) (Pos 'e' 4) 2) `shouldBe` (True :: Bool)-- the tootall is in f6, should be moved first


        it "FAIL TOOO TALL blue fails to make the stack <=4 " $ do
            let b = [[Stack [Red,Red,Red,Red, Red, Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue, Blue, Blue,Blue],Stack [Blue, Blue,Blue, Blue, Blue, Blue]]]
                in isValidMove b (Move (Pos 'f' 6) (Pos 'f' 5) 1) `shouldBe` (False :: Bool)-- the tootall is in f6, makes the stack >4 which is not small enough
        

         
        --complex

        it " moving out of the board" $ do
            let b = [[Stack [Red,Red,Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]]
                in isValidMove b (Move (Pos 'a' 1) (Pos 'a' 0) 1) `shouldBe` (False :: Bool)   


        it " true moving in a non straight line red" $ do
            let b = [[Stack [Red,Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]]
                in isValidMove b (Move (Pos 'a' 1) (Pos 'c' 3) 2) `shouldBe` (True :: Bool)

        
        it " b1 stack of 4 bouncing on the boarder to go with 3 steps to c4 true " $ do
            let b = [[Stack [Red,Red,Red,Red],Stack [Red,Red,Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]]
                in isValidMove b (Move (Pos 'b' 1) (Pos 'c' 4) 3) `shouldBe` (True :: Bool)   

        
        it "stay in the place fail" $ do
            let b = [[Stack [Red,Red,Red,Red,Red,Red,Red,Red,Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue, Blue, Blue,Blue],Stack [Blue, Blue,Blue, Blue, Blue, Blue]]]
                in isValidMove b (Move (Pos 'a' 1) (Pos 'a' 1) 10) `shouldBe` (False :: Bool)-- the tootall is in f6, makes the stack >4 which is not small enough
        



testListMoves :: Spec
testListMoves = describe "IF Validate-Module-Game: listMoves ..." $ do
        it "red cannot move" $ do
            let board = [[Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]]
                result = []
                in
                    map show (listMoves board Red) `shouldMatchList` (result :: [String])    
    
        it "basic movement for Red" $ do
            let board = [[Stack [Red],Empty,Empty,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty],
                        [Stack [Blue],Empty,Empty,Empty,Empty,Empty]]
                result = ["a1-1-a2", "a1-1-b2", "a1-1-b1"]
            map show (listMoves board Red) `shouldMatchList` result

        it "diagonal movement for Red" $ do
            let board = [[Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Stack [Red],Empty,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty]]
                result =  ["b2-1-b3", "b2-1-c3", "b2-1-c2", "b2-1-c1", "b2-1-b1", "b2-1-a1", "b2-1-a2", "b2-1-a3"]
            map show (listMoves board Red) `shouldMatchList` result

        it "movement for Red at b2 perseritje kot" $ do
            let board = [[Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Stack [Red],Empty,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty]]
                result =  ["b2-1-b3", "b2-1-c3", "b2-1-c2", "b2-1-c1", "b2-1-b1", "b2-1-a1", "b2-1-a2", "b2-1-a3"]
            map show (listMoves board Red) `shouldMatchList` result

