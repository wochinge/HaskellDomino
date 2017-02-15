module Lib (
    initialGameState,
    nextMove,
    GameState
)
    where

import Prelude hiding (round)
import Data.Maybe (isJust, fromJust)
import Data.List (mapAccumL, findIndex, minimumBy)
import Data.Bool (bool)

data Stone = Stone Int Int
                deriving (Eq, Show)
type DirectedStone = Stone
type Snake = [DirectedStone]

type NumberOfSkips = Int
data GameState = GameState Snake [Hand] NumberOfSkips

data Hand = Hand Player [Stone] deriving (Show)

data MoveResult = Skip | Play | Win deriving (Eq)
data Result = Result Snake Hand MoveResult

type Player = String
type Score = Int

mkStoneSet :: [Stone]
mkStoneSet = [Stone x y | x <- [0 .. 6], y <- [x .. 6]]

deal :: [Stone] -> [String] -> [Hand]
deal stones players =
    let nrOfPlayers = length players
    in zipWith Hand players $ chunks stones nrOfPlayers

chunks :: [a] -> Int -> [[a]]
chunks xs nrOfChunks =
    partitionBy chunkSize xs
    where
        chunkSize = (length xs) `div` nrOfChunks
        partitionBy _ [] = []
        partitionBy n xs = take n xs : partitionBy n (drop n xs)

initSnake :: Snake
initSnake = []

game :: [Hand] -> Player
game hs = gameLoop $ GameState initSnake hs 0

gameLoop :: GameState -> Player
gameLoop gameState
    | isJust winner = fromJust winner
    | otherwise = gameLoop updatedGameState
    where
        (winner, updatedGameState) = round gameState

round :: GameState -> (Maybe Player, GameState)
round (GameState snake hands numberOfSkips)
    | weHaveAWin = (Just currentPlayer, updatedState)
    | weHaveAStall = (Just winnerPlayer, updatedState)
    | otherwise = (Nothing, updatedState)
    where
        currentHand@(Hand currentPlayer _) = head hands
        (Result updatedSnake updatedHand moveResult) = move snake currentHand
        weHaveAWin = moveResult == Win
        weHaveAStall = newSkips == length hands
        winnerPlayer = findWinningPlayer hands
        newSkips = bool 0 (numberOfSkips + 1 ) $ moveResult == Skip
        updatedState = GameState updatedSnake updatedHands newSkips
        updatedHands = tail hands ++ [updatedHand]

move :: Snake -> Hand -> Result
move = undefined
-- move [] (Hand player (stone:rest)) = Result [stone] (Hand player rest) Play
-- move snake (Hand player stones) =
--     let Stone(startDots, _) = head snake
--         Stone(_, endDots) = last snake
--
--
--         x = map  [head snake, last snake]
--
--         frontMatches =


data Fit = EndDirect | EndReverse | StartDirect | StartReverse deriving Show

genPossibilities :: Int -> Int -> [(Stone, Fit)]
genPossibilities start end = (genPoss end EndDirect EndReverse) ++ (genPoss start StartReverse StartDirect)

genPoss :: Int -> Fit -> Fit -> [(Stone, Fit)]
genPoss dots ifFirst ifLast = [ ((Stone dots x) , ifFirst) | x <- [dots..6]] ++ [ ((Stone x dots) , ifLast) | x <- [0..dots]]
-- stoneMatches :: Stone -> [Stones] -> DirectedStone
-- stoneMatches (Stone front back) ()

deleteFirstBy :: (a -> Bool) -> [a] -> (Maybe a, [a])
deleteFirstBy _ [] = (Nothing, [])
deleteFirstBy predicate (x:xs)
    | predicate x = (Just x, xs)
    | otherwise = (result, x:ys)
    where (result, ys) = deleteFirstBy predicate xs

findWinningPlayer :: [Hand] -> Player
findWinningPlayer hands =
    let sums = map (\(Hand name stones) -> (name, sumHand stones)) hands
    in fst $ minimumBy (\(_, score1) (_, score2) -> compare score1 score2) sums

sumHand :: [Stone] -> Score
sumHand [] = 0
sumHand ((Stone a b):stones) = a + b + sumHand stones
