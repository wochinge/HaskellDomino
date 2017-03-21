module Play
( GameState(..)
, Hand(..)
, Stone(..)
, Snake
, isDouble
, play
, mkStoneSet
) where

import Data.Function((&))
import Data.List(delete, find)
import Data.Maybe(isJust, fromJust)
import Debug.Hood.Observe
import Generics.Deriving
import Control.DeepSeq

data Stone = Stone
  { first  ::Int
  , second :: Int
} deriving (Eq, Show, Generic, NFData)
instance Observable Stone

mkStoneSet :: Int -> Int -> [Stone]
mkStoneSet min max = [Stone x y | x <- [min .. max], y <- [x .. max]]

type Snake = [Stone]

initSnake :: Snake
initSnake = []

data Hand player = Hand
  { player :: player
  , stones :: [Stone]
  } deriving (Show, Generic)
instance Observable player => Observable (Hand player)

type NumberOfSkips = Int
data GameState player = GameState
  { snake :: Snake
  , hands :: [Hand player]
  , skips:: NumberOfSkips
  } deriving (Show,Generic)
instance Observable player => Observable (GameState player)

chunks :: [a] -> Int -> [[a]]
chunks xs nrOfChunks =
    partitionBy chunkSize xs
    where
        chunkSize = length xs `div` nrOfChunks
        partitionBy _ [] = []
        partitionBy n xs = take n xs : partitionBy n (drop n xs)

initGameState :: [player] -> [Stone] -> GameState player
initGameState ps ss = GameState initSnake hs 0
  where
    hs = zipWith Hand ps $ chunks ss $ length ps

bothResultsEqual :: Eq b => (a->b) -> (a->b) -> a -> Bool
bothResultsEqual f g x = f x == g x

getSums :: GameState player -> [Int]
getSums  = map (sumStones . stones) . hands

isStalled :: GameState player -> Bool
isStalled = bothResultsEqual skips (length.hands)

isStalledWinner :: GameState player -> Bool
isStalledWinner state = isStalled state && bothResultsEqual minimum head sums
  where sums = getSums state

isWinner :: GameState player -> Bool
isWinner  = null . stones . head . hands

isComplete :: GameState player -> Bool
isComplete state = any (state &) [isWinner , isStalled]

sumStones :: [Stone] -> Int
sumStones = sum . map sumOneStone
  where sumOneStone (Stone a b) = a + b

data MoveOutput = Skip | Play | Win deriving (Eq, Show)

data MoveResult player = MoveResult
  { snakeOut :: Snake
  , hand :: Hand player
  , output :: MoveOutput
  } deriving Show

data Fit = EndDirect | EndReverse | StartDirect | StartReverse deriving (Eq, Show)

data Fitted s = FittedStone
    { fitting :: Fit
    , piece :: s
    }

genPossibilities :: Int -> Int -> [Fitted Stone]
genPossibilities start end =
          genPoss end EndDirect EndReverse ++
          genPoss start StartReverse StartDirect
  where
    genPoss dots ifFirst ifLast =
      [ FittedStone ifFirst (Stone dots x) | x <- [dots..6]] ++
      [ FittedStone ifLast (Stone x dots) | x <- [0..dots]]

addStone :: Fitted Stone -> Snake -> Snake
addStone (FittedStone fit stone@(Stone a b)) snake
  | fit == EndDirect = snake ++ [stone]
  | fit == EndReverse = snake ++ [Stone b a]
  | fit == StartDirect = stone : snake
  | fit == StartReverse = Stone b a : snake

class MayHaveDouble a where
    isDouble :: a -> Bool

instance MayHaveDouble Stone where
    isDouble s = first s == second s

instance MayHaveDouble (Fitted Stone) where
    isDouble = isDouble.piece

preferDoubles :: MayHaveDouble a => [a] -> a
preferDoubles ss
    | isJust double = fromJust double
    | otherwise = head ss
    where
        double = find isDouble ss

move :: Snake -> Hand player -> MoveResult player
move [] hand = MoveResult [selectedStone] outputHand Play
  where
    outputHand = Hand (player hand)  $ delete selectedStone $ stones hand
    selectedStone = preferDoubles $ stones hand
move snake hand
  | null (stones hand) = MoveResult snake hand Win
  | null validMoves = MoveResult snake hand Skip
  | null ( stones outputHand ) = MoveResult newSnake outputHand Win
  | otherwise = MoveResult newSnake outputHand Play
  where
    possibilities = genPossibilities ((first.head) snake) $ (second.last) snake
    stonesIn = stones hand
    validMoves = filter ((`elem` stonesIn).piece) possibilities
    selectedMove = preferDoubles validMoves
    outputHand = Hand (player hand) $ delete (piece selectedMove) stonesIn
    newSnake = addStone selectedMove snake

nextMove :: GameState player -> GameState player
nextMove state
  | hasWon = GameState oldSnake oldHands (-1)
  | isStalled state = GameState oldSnake rotatedHands oldSkips
  | otherwise = GameState newSnake newRotatedHands newSkips
  where
    oldSnake = snake state
    oldHands = hands state
    thisHand = head oldHands
    rotatedHands = tail oldHands ++ [thisHand]
    oldSkips = skips state
    hasWon = isWinner state || isStalledWinner state
    result = move oldSnake thisHand
    newSnake = snakeOut result
    newRotatedHands = if output result == Win
                          then hand result : tail oldHands
                          else tail oldHands ++ [hand result]
    newSkips = if output result == Skip then oldSkips+1 else 0

isValid :: GameState player -> Bool
isValid state = skips state >= 0

movements :: GameState player -> [GameState player]
movements initial = moves
  where moves = initial : map nextMove moves

usefulMovements :: [GameState player] -> [GameState player]
usefulMovements = takeWhile isValid

play :: [player] -> [Stone] -> [GameState player]
play players stones = usefulMovements $ movements $ initGameState players stones
