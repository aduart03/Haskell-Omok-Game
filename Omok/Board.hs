-- Board.hs

-- This module provides functions for managing the game board.
module Board where

type Board = [[String]]  -- Change the type to use String instead of Int

-- Board initializer
mkBoard :: Int -> Board
mkBoard n = replicate n (replicate n ". ")

-- Board size
size :: Board -> Int
size bd = length bd

-- Display board to console
printBoard :: Board -> IO ()
printBoard board = do
    printBoardHeader
    mapM_ (\(rowNum, row) -> putStrLn $ formatRow rowNum row) $ zip [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5] board

-- Format row of board
formatRow :: Int -> [String] -> String
formatRow rowNum row = show rowNum ++ "| " ++ concat row

-- displays board header to the console
printBoardHeader :: IO ()
printBoardHeader = putStrLn "   1 2 3 4 5 6 7 8 9 0 1 2 3 4 5\n   -----------------------------"

-- Updates board based on player, opponent, and computer moves, then displays to the console
updateBoard :: Board -> Int -> Int -> String -> Board  -- Accept String instead of Int
updateBoard board row col value =
    let (before, currentRow : after) = splitAt (row - 1) board
        (left, _ : right) = splitAt (col - 1) currentRow 
    in before ++ [left ++ [value] ++ right] ++ after

-- Checks if a position or cell is empty
isEmpty :: Board -> Int -> Int -> Bool
isEmpty board row col = (board !! (row - 1)) !! (col - 1) == ". "

-- Checks if the x and y specified is valid
isValidInput :: Int -> Bool
isValidInput x = x >= 1 && x <= 15

-- Function to check for a win condition
checkWin :: Board -> Int -> Int -> String -> Bool
checkWin board row col marker =
    checkHorizontalWin board row col marker ||
    checkVerticalWin board row col marker ||
    checkDiagonalWin board row col marker

-- Function to check for a win condition horizontally
checkHorizontalWin :: Board -> Int -> Int -> String -> Bool
checkHorizontalWin board row col marker =
    let rowMarkers = board !! (row - 1)
        consecutiveMarkers = filter (== marker) rowMarkers
    in length consecutiveMarkers >= 5

-- Function to check for a win condition vertically
checkVerticalWin :: Board -> Int -> Int -> String -> Bool
checkVerticalWin board row col marker =
    let columnMarkers = map (!! (col - 1)) board
        consecutiveMarkers = filter (== marker) columnMarkers
    in length consecutiveMarkers >= 5

-- Function to check for a win condition diagonally
checkDiagonalWin :: Board -> Int -> Int -> String -> Bool
checkDiagonalWin board row col marker =
    let diagonalMarkers1 = map (\(r, c) -> board !! (r - 1) !! (c - 1)) [(r, col - (row - r)) | r <- [1..15], col - (row - r) >= 1 && col - (row - r) <= 15]
        diagonalMarkers2 = map (\(r, c) -> board !! (r - 1) !! (c - 1)) [(r, col + (row - r)) | r <- [1..15], col + (row - r) >= 1 && col + (row - r) <= 15]
        consecutiveMarkers1 = filter (== marker) diagonalMarkers1
        consecutiveMarkers2 = filter (== marker) diagonalMarkers2
    in length consecutiveMarkers1 >= 5 || length consecutiveMarkers2 >= 5
    