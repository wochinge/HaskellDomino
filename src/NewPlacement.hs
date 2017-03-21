module NewPlacement
( newPlacement
, ViewableSnake(..)
, initialViewableSnake
, Positioning(..)
, Positioned
, PositionedStone
) where

import Play
    ( GameState(..)
    , Hand(..)
    , Stone(..)
    , Snake
    , isDouble
    )
import Data.Maybe(fromJust)
import Graf
import VisibleStone( heightStone, widthStone )
import Debug.Hood.Observe
import Generics.Deriving
import Control.DeepSeq

data Positioning = Upwards | Downwards | Leftwards | Rightwards deriving (Show,Generic,NFData)
instance Observable Positioning

type Positioned = (Point,Positioning)

type PositionedStone = (Positioned, Stone)

data ViewableSnake = ViewableSnake
  { center :: Maybe PositionedStone
  , leftWing :: [PositionedStone]
  , rightWing :: [PositionedStone]
  , centerIndex :: Int
  , lastLength :: Int
  } deriving (Show,Generic, NFData)
instance Observable ViewableSnake

initialViewableSnake :: ViewableSnake
initialViewableSnake = ViewableSnake Nothing [] [] 0 0

calculatePlacement :: Float -> Float -> PositionedStone -> Stone -> PositionedStone
calculatePlacement mult _ pre st = (((x,0),how), st)
    where
        reference = (fst.fst.fst) pre
        (how,movement) = if isDouble st then (Upwards,heightStone/2) else (Rightwards,widthStone/2)
        otherMovement = if isDouble $ snd pre then heightStone/2 else widthStone/2
        x =  reference + mult * (movement + otherMovement + 2)

newPlacement :: Float -> GameState player -> ViewableSnake -> ViewableSnake
newPlacement _ (GameState [] _ _ ) vs = vs
newPlacement _ gs (ViewableSnake _ _ _ _ 0) = ViewableSnake (Just newPos) [] [] 0 1
  where
    stone = head $ snake gs
    how = if isDouble stone then Upwards else Rightwards
    newPos = (((0,0),how), stone)
newPlacement tableRadius gs vs@(ViewableSnake central ll rl pos len)
  | doNothing = vs
  | gotoRight = ViewableSnake central ll (postPos:rl) pos (len+1)
  | otherwise = ViewableSnake central (prePos:ll) rl (pos+1) (len+1)
  where
    snk = snake gs
    doNothing = length snk == len
    located = snk !! pos
    theCenter = fromJust central
    gotoRight = located == snd theCenter
    preStone = if (not.null) ll then head ll else theCenter
    prePos = calculatePlacement (-1) tableRadius preStone $ head snk
    postStone = if (not.null) rl then head rl else theCenter
    postPos = calculatePlacement 1 tableRadius postStone $ last snk
