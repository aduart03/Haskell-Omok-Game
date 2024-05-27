-- Main.hs

-- This is the main file which will execute IO actions.

module Main where

import System.IO
import Board
import Model
import VC
import RandomStrategy

main :: IO ()
main = do
    -- Prompts user to choose between playing against a human opponent or computer
    opponent <- chooseOpponent

    -- 1: if human 2. if computer 3. if none -> exit program
    if opponent == "1"
        then do 
            -- Initialize an empty board 
            let initialBoard = mkBoard 15
            caseHandler "h"
            -- Display the initial board
            printBoard initialBoard
            -- Start the game loop
            gameLoop initialBoard count
        else do
            if opponent == "2" 
                then do
                    -- Initialize an empty board 
                    let initialBoard = mkBoard 15
                    -- Display the initial board
                    caseHandler "c"
                    printBoard initialBoard
                    -- Start the game loop Random Strategy
                    gameLoopRandomStrategy initialBoard count
                else do 
                    if opponent == "3"
                        then do
                            caseHandler "e"
                            else do
                                caseHandler ""
                                main

gameLoop :: Board -> Int -> IO ()
gameLoop board count = do
    -- Player, Opponent, and Current player
    let mkPlayer = "x "
    let mkOpponent = "o "
    let tempPlayer = if count `mod` 2 == 0 then mkPlayer else mkOpponent  -- Define tempPlayer based on count
    -- Prompt the user to enter row and column indices
    input <- typeInput
    if input == "e"  -- Check if the input is 'e'
        then 
        caseHandler "e"
        else do
            let inputValues = words input
            -- Check if both row and column indices are provided
            if length inputValues /= 2 || not (isValidInputString (head inputValues)) || not (isValidInputString (last inputValues))
                then do
                    caseHandler "b"
                    gameLoop board count
                else do
                    let [row, col] = map read inputValues :: [Int]
                    -- Check if the provided row and column indices are within the valid range
                    if isValidInput row && isValidInput col
                        then do
                            -- Check if the selected position is within the bounds of the board
                            if row >= 1 && row <= 15 && col >= 1 && col < 15
                                then do
                                    -- Check if the selected position is empty
                                    if isEmpty board col row
                                        then do
                                            -- Update the board with 'X' or 'O' based on the count
                                            let updatedBoard = if count `mod` 2 == 0 
                                                                then updateBoard board col row mkPlayer
                                                                else updateBoard board col row mkOpponent
                                            -- Display the updated board
                                            caseHandler "u"
                                            printBoard updatedBoard
                                            
                                            -- Check if the player has won
                                            if checkWin updatedBoard col row tempPlayer
                                                then do 
                                                    player <- printWinner count
                                                    putStrLn $ show player
                                                else do
                                                    -- Continue the game loop with incremented count
                                                    gameLoop updatedBoard (count + 1)
                                        else do
                                            -- If the position is not empty, inform the user and call gameLoop again without incrementing count
                                            caseHandler "t"
                                            gameLoop board count
                                else do
                                    -- If the position is out of bounds, inform the user and call gameLoop again without incrementing count
                                    caseHandler "o"
                                    gameLoop board count
                        else do
                            -- If the input is invalid, inform the user and call gameLoop again without incrementing count
                            caseHandler "i"
                            gameLoop board count