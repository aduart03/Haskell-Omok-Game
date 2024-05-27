-- Model.hs

-- This is the model file which will store or generate data that the VC will use to display to the console.

module Model where

import System.Random
import VC

-- Function to check if the input is within the valid range (1 to 15)
count :: Int --determines o or x player
count = 0

-- Function to generate a random number within a given range [low, high]
getRandomNumberRow :: Int -> Int -> IO Int
getRandomNumberRow low high = do
    randomNumber <- randomRIO(low,high)
    return randomNumber

-- Function to generate a random number within a given range [low, high]
getRandomNumberCol :: Int -> Int -> IO Int
getRandomNumberCol low high = do
    randomNumber <- randomRIO(low,high)
    return randomNumber
