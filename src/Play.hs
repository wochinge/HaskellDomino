module Play
  (
  mkStoneSet, play, GameState
  )
where

data Stone = Stone Int Int
                deriving (Eq, Show)

mkStoneSet :: Int -> Int -> [Stone]
mkStoneSet min max = [Stone x y | x <- [min .. max], y <- [x .. max]]

type DirectedStone = Stone
type Snake = [DirectedStone]

initSnake :: Snake
initSnake = []

data Hand player = Hand
  { player :: player
  , stones :: [Stone]
  } deriving (Show)

type NumberOfSkips = Int
data GameState player = GameState
  { snake :: Snake
  , hands :: [Hand player]
  , skips:: NumberOfSkips
  } deriving Show

chunks :: [a] -> Int -> [[a]]
chunks xs nrOfChunks =
    partitionBy chunkSize xs
    where
        chunkSize = (length xs) `div` nrOfChunks
        partitionBy _ [] = []
        partitionBy n xs = take n xs : partitionBy n (drop n xs)

initGameState :: [player] -> [Stone] -> GameState player
initGameState ps ss = GameState initSnake hs 0
  where
    hs = zipWith Hand ps $ chunks ss $ length ps

isStalled :: GameState player -> Bool
isStalled state = ( skips state ) == ( length (hands state) )

isComplete :: GameState player -> Bool
isComplete state = won || (isStalled state)
  where
    won = null $ stones $  ((hands state) !! 0)

nextMove :: GameState player -> GameState player
nextMove state
  | hasWon = state
  | isStalled = GameState oldSnake rotatedHands oldSkips
  | otherwise = GameState newSnake newRotatedHands newSkips
  where
    oldSnake = snake state
    oldHands = hands state
    rotatedHands = undefined
    oldSkips = skips state
    hasWon = undefined
    isStalled = undefined
    newSnake = undefined
    newRotatedHands = undefined
    newSkips = undefined

movements :: GameState player -> [GameState player]
movements initial = moves
  where moves = initial : (map nextMove moves)

usefulMovements :: [GameState player] -> [GameState player]
usefulMovements = takeWhile (not.isComplete)

play :: [player] -> [Stone] -> [GameState player]
play players stones = usefulMovements $ movements $ initGameState players stones
