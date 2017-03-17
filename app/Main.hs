module Main where

import Play(mkStoneSet, play)
import View(showGame)
import System.Random.Shuffle (shuffleM)

-- players :: [Int]
-- players = map (show. (1111*)) [1..numPlayers]
players = ["Alice", "Bob", "Charlie", "Demian", "Eleanor", "Fedora", "Grace"]

minStoneHalfValue:: Int
minStoneHalfValue = 0

maxStoneHalfValue:: Int
maxStoneHalfValue = 6

main :: IO ()
main = do
    randomStones <- shuffleM $ mkStoneSet minStoneHalfValue maxStoneHalfValue
    showGame $ play players randomStones
