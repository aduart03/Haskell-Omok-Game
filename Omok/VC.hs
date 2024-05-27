-- VC.hs

-- Console based user interface.

module VC where
    
import Data.Char (isDigit)  -- Checks if strings are intergers

-- Define chooseOpponent at the top level
chooseOpponent :: IO String
chooseOpponent = do
    putStrLn "\nPlay against human player(1) or computer (2)? Or exit game (3)"
    getLine

-- Reads user input
typeInput :: IO String
typeInput = do
    putStrLn "Enter row and column indices (separated by a space) or 'e' to exit the game:"
    getLine

-- Function to validate input strings
isValidInputString :: String -> Bool
isValidInputString = all isDigit

-- Prompts messages based on if block and cases
caseHandler :: String -> IO ()
caseHandler e = case e of
    "e" -> putStrLn "Exiting the game."
    "b" -> putStrLn "Invalid input. Please enter both row and column indices."
    "u" -> putStrLn "Updated Board:"
    "w" -> putStrLn "Congrats You Won! Ending game..."
    "l" -> putStrLn "You Loose! Computer Won game. Ending game..."
    "t" -> putStrLn "Invalid Position! Already Taken"
    "o" -> putStrLn "Invalid Position! Row or Column out of bounds"
    "i" -> putStrLn "Invalid input. Please enter valid row and column indices."
    "h" -> putStrLn "Playing against human opponent...\nInitial Board:\n"
    "c" -> putStrLn "Playing against Computer\nChoosing Random Strategy...\nInitial Board:\n"
    "d" -> putStrLn "Computer is making a move..."
    _   -> putStrLn "No input or invalid input. Try again."

-- Returns player move and computer move to the console every time a player or opponent makes a move
moves :: String -> Int -> Int -> IO String
moves m row col = case m of
    "move" -> do
        putStrLn $ "Your Move: [" ++ show row ++ "," ++ show col ++ "] -> 'x'"
        return "x"
    "cmove" -> do
        putStrLn $ "Your Move: [" ++ show row ++ "," ++ show col ++ "] -> 'o'"
        return "o"
    _ -> do
        putStrLn "Invalid move"
        return ""

-- Returns the winner of the game based on the current player and checkWin method. Usually used within the chekWin method.
printWinner :: Int -> IO String
printWinner count =
    if count `mod` 2 == 0 
            then return "Player 1 Wins! Ending game..."
            else return "Player 2 Wins! Ending game..."
