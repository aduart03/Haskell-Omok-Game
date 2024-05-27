-- RandomStrategy.hs

-- This is the random strategy logic.

module RandomStrategy where

import System.IO
import Board
import VC
import System.Random
import Model

gameLoopRandomStrategy :: Board -> Int -> IO ()
gameLoopRandomStrategy board count = do
    -- Player, Opponent, and Current player
    let mkPlayer = "x "
    let mkOpponent = "o "
    let tempPlayer = if count `mod` 2 == 0 then mkPlayer else mkOpponent  -- Define tempPlayer based on count

    if count `mod` 2 == 0 
        then do
            -- Prompt the user to enter row and column indices
            input <- typeInput
            if input == "e"  -- Check if the input is 'e'
                then caseHandler "e"
                else do
                    let inputValues = words input
                    -- Check if both row and column indices are provided
                    if length inputValues /= 2 || not (isValidInputString (head inputValues)) || not (isValidInputString (last inputValues))
                        then do
                            caseHandler "b"
                            gameLoopRandomStrategy board count
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
                                                    let updatedBoard = updateBoard board col row mkPlayer
                                                    printBoard updatedBoard
                                                    if checkWin updatedBoard col row tempPlayer 
                                                        then do  
                                                            -- Win agianst computer message
                                                            caseHandler "w"
                                                        else do
                                                            -- Displays computer and player move
                                                            moves "move" row col
                                                            gameLoopRandomStrategy updatedBoard (count + 1)
                                                else do
                                                    -- If the position is not empty, inform the user and call gameLoop again without incrementing count
                                                    caseHandler "t"
                                                    gameLoopRandomStrategy board count
                                        else do
                                            -- If the position is out of bounds, inform the user and call gameLoop again without incrementing count
                                            caseHandler "o"
                                            gameLoopRandomStrategy board count
                                else do
                                    -- If the input is invalid, inform the user and call gameLoop again without incrementing count
                                    caseHandler "i"
                                    gameLoopRandomStrategy board count
    else do
        -- Computer making move message
        caseHandler "d"
        -- Generates random numbers based on the System.Random package
        comRow <- getRandomNumberRow 1 15
        comCol <- getRandomNumberCol 1 15
        if isValidInput comRow && isValidInput comCol
            then do
                -- Check if the selected position is within the bounds of the board
                if comRow >= 1 && comRow <= 15 && comCol >= 1 && comCol < 15
                    then do
                        -- Check if the selected position is empty
                        if isEmpty board comCol comRow
                            then do
                                -- Update the board with 'X' or 'O' based on the count
                                let updatedBoard = updateBoard board comCol comRow mkOpponent
                                printBoard updatedBoard
                                if checkWin updatedBoard comCol comRow tempPlayer 
                                    then do  
                                        -- Lost against computer
                                        caseHandler "l"
                                    else do
                                        -- Displays computer and player move
                                        moves "cmove" comRow comCol
                                        gameLoopRandomStrategy updatedBoard (count + 1)
                            else do
                                -- If the position is not empty, inform the user and call gameLoop again without incrementing count
                                caseHandler "t"
                                gameLoopRandomStrategy board count
                    else do
                        -- If the position is out of bounds, inform the user and call gameLoop again without incrementing count
                        caseHandler "o"
                        gameLoopRandomStrategy board count
            else do
                -- If the input is invalid, inform the user and call gameLoop again without incrementing count
                caseHandler "i"
                gameLoopRandomStrategy board count
