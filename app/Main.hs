module Main
( main
) where

import Play(mkStoneSet, play)
import View(showGame)
import System.Random.Shuffle (shuffleM)
import Debug.Hood.Observe

-- players :: [Int]
-- players = map (show. (1111*)) [1..numPlayers]
players = ["Alice", "Bob", "Charlie", "Demian", "Eleanor", "Fedora", "Grace"]

minStoneHalfValue:: Int
minStoneHalfValue = 0

maxStoneHalfValue:: Int
maxStoneHalfValue = 6

main :: IO ()
main = runO $ do
    randomStones <- shuffleM $ mkStoneSet minStoneHalfValue maxStoneHalfValue
    -- print $ last $ play players randomStones
    showGame $ play players randomStones
