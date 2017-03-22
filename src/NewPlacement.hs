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
import VisibleStone( heightStone, widthStone )
import Graphics.Gloss

data Positioning = Upwards | Downwards | Leftwards | Rightwards deriving (Show)

type Positioned = (Point,Positioning)

type PositionedStone = (Positioned, Stone)

data ViewableSnake = ViewableSnake
  { center :: Maybe PositionedStone
  , leftWing :: [PositionedStone]
  , rightWing :: [PositionedStone]
  , centerIndex :: Int
  , lastLength :: Int
  } deriving (Show)

type PointChange = Point

halfWidthStone = widthStone/2
halfHeightStone = heightStone/2

initialViewableSnake :: ViewableSnake
initialViewableSnake = ViewableSnake Nothing [] [] 0 0

invert :: PointChange -> PointChange
invert (x,y) = (-x,-y)

mayInvert :: Bool -> PointChange -> PointChange
mayInvert True = id
mayInvert False = invert

getDisplacement :: Bool -> Positioning -> PointChange
getDisplacement post Leftwards = mayInvert post (halfWidthStone,0)
getDisplacement post Rightwards = mayInvert (not post) (halfWidthStone,0)
getDisplacement post Upwards = mayInvert (not post) (0,halfWidthStone)

getDoubleDisplacement :: Bool -> Positioning -> PointChange
getDoubleDisplacement post Upwards = mayInvert (not post) (halfHeightStone,0)
getDoubleDisplacement post Downwards = mayInvert (not post) (halfHeightStone,0)
getDoubleDisplacement post Leftwards = mayInvert post (0,halfHeightStone)

getMovement :: Bool -> PositionedStone -> PointChange
getMovement postpend ps = proposed
    where
        stone = snd ps
        gotDouble = isDouble stone
        proposed = (if gotDouble then getDoubleDisplacement else getDisplacement) postpend $ (snd.fst) ps

getBendDisplacement :: Bool -> Positioning -> PointChange
getBendDisplacement post Leftwards = mayInvert (not post) (0,halfHeightStone)
getBendDisplacement post Rightwards = mayInvert (not post) (0,halfHeightStone)
getBendDisplacement post Upwards = mayInvert post (halfHeightStone,0)

getBendDoubleDisplacement :: Bool -> Positioning -> PointChange
getBendDoubleDisplacement post Upwards = mayInvert (not post) (0,halfWidthStone)
getBendDoubleDisplacement post Downwards = mayInvert post (0,halfWidthStone)
getBendDoubleDisplacement post Leftwards = mayInvert (not post) (halfWidthStone,0)

getBendMovement :: Bool -> PositionedStone -> PointChange
getBendMovement postpend ps = proposed
    where
        stone = snd ps
        gotDouble = isDouble stone
        proposed = (if gotDouble then getBendDoubleDisplacement else getBendDisplacement) postpend $ (snd.fst) ps

rotateFW :: Positioning -> Positioning
rotateFW Upwards = Leftwards
rotateFW Leftwards = Downwards
rotateFW Downwards = Rightwards
rotateFW Rightwards = Upwards

rotateBW :: Positioning -> Positioning
rotateBW Upwards = Rightwards
rotateBW Leftwards = Upwards
rotateBW Downwards = Leftwards
rotateBW Rightwards = Downwards

getStraightPositioning :: Bool -> Positioning -> Positioning
getStraightPositioning False = id
getStraightPositioning True = rotateFW

getDoublePositioning :: Bool -> Positioning -> Positioning
getDoublePositioning True = id
getDoublePositioning False = rotateBW

getPositionedStone :: PositionedStone -> Stone -> PositionedStone
getPositionedStone ((point,positioning),value) stone
    | isDouble stone = ((point,getDoublePositioning (isDouble value) positioning),stone)
    | otherwise = ((point,getStraightPositioning (isDouble value) positioning),stone)

getBendPositionedStone :: Bool -> PositionedStone -> Stone -> PositionedStone
getBendPositionedStone True ((point,positioning),value) stone
    | isDouble stone = ((point,getPostDoubleBendPositioning (isDouble value) positioning),stone)
    | otherwise = ((point,getPostStraightBendPositioning (isDouble value) positioning),stone)
getBendPositionedStone False ((point,positioning),value) stone
    | isDouble stone = ((point,getPreDoubleBendPositioning (isDouble value) positioning),stone)
    | otherwise = ((point,getPreStraightBendPositioning (isDouble value) positioning),stone)



displaceBy :: Point -> Point -> Point
displaceBy (x1,y1) (x2,y2) = (x1+x2,y1+y2)

calculatePlacement :: Bool -> Float -> PositionedStone -> Stone -> PositionedStone
calculatePlacement postpend radius pre st = selectedPos
        where
        var1 = getMovement postpend pre
        ps = getPositionedStone pre st
        var2 = getMovement postpend ps
        var = var1 `displaceBy` var2
        pos = (fst.fst) pre `displaceBy` var
        overflows = ((^2).fst) pos + ((^2).snd) pos > radius
        var3 = getBendMovement postpend pre
        psBend = getBendPositionedStone postpend pre st
        var4 = getBendMovement postpend psBend
        varBend = var3 `displaceBy` var4
        posBend = (fst.fst) pre `displaceBy` varBend
        (selectedDst,selectedPs) = if overflows then (posBend,psBend) else (pos,ps)
        selectedPos = ((selectedDst,(snd.fst) selectedPs),st)

        --referencePos = (fst.fst) pre

    -- where
    --     reference = (fst.fst.fst) pre
    --
    --     (how,movement) = if isDouble st then (Upwards,heightStone/2) else (Rightwards,widthStone/2)
    --     otherMovement = if isDouble $ snd pre then heightStone/2 else widthStone/2
    --     x =  reference + mult * (movement + otherMovement + 2)
    --     proposed = correctPlacement mult, radius (((x,0),how), st)

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
    prePos = calculatePlacement False tableRadius preStone $ head snk
    postStone = if (not.null) rl then head rl else theCenter
    postPos = calculatePlacement True tableRadius postStone $ last snk
