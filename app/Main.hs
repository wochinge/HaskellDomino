module Main where

import Play(mkStoneSet, play)
import View(showGame)
import System.Random.Shuffle (shuffleM)

-- TODO allow for different configurations
numPlayers:: Int
numPlayers = 4

-- players :: [Int]
players = map show [1..numPlayers]

minStoneHalfValue:: Int
minStoneHalfValue = 0

maxStoneHalfValue:: Int
maxStoneHalfValue = 6

main :: IO ()
main = do
    randomStones <- shuffleM $ mkStoneSet minStoneHalfValue maxStoneHalfValue
    showGame $ play players randomStones
