module Main where

import Lib(game, mkStoneSet, deal)
import System.Random.Shuffle (shuffleM)

main :: IO ()
main = do
    randomStones <- shuffleM mkStoneSet
    let players = map show [0..3]
    let initialHands = deal randomStones players
    putStrLn "And the winner is..."
    putStrLn . show $ game initialHands
