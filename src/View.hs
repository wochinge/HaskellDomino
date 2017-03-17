module View (
  showGame
) where

import Data.List(intercalate, genericLength, find)
import Data.Maybe(fromJust)
import Graphics.Gloss
import Play
    ( GameState(..)
    , Hand(..)
    , Stone(..)
    , Snake
    , Hand
    , isDouble
    )
import VisibleStone

class ShowUnquoted a where
  showWithoutQuotes :: a -> String
  default showWithoutQuotes :: Show a => a -> String
  showWithoutQuotes = show

instance ShowUnquoted String where
  showWithoutQuotes = id

data LocatedPlayer player = LocatedPlayer
    { label :: player
    , position :: Point
    , angle :: Float
    }

data ViewableSnake = ViewableSnake
  { leftWing :: [PositionedStone]
  , rightWing :: [PositionedStone]
  , centerIndex :: Int
  , lastLength :: Int
  } deriving Show

data ViewState player = ViewState
    { steps :: [GameState player]
    , viewable :: ViewableSnake
    , players :: [LocatedPlayer player]
    , winner :: Maybe player
    }

data Positioning = Upwards | Downwards | Leftwards | Rightwards deriving Show

type Positioned = (Point,Positioning)

-- Constants for painting
tableRadius = 275 :: Float

playerRadius = 350 :: Float

windowSizeX = 900 :: Int

windowSizeY = 900 :: Int

handStonePositions :: [Point]
handStonePositions =zip posX $ repeat (-50)
  where posX = 0 : concatMap (\x -> [-x,x]) [25,50..]

paintLocatedStone :: Positioned -> Stone -> Picture
paintLocatedStone ( (x,y), Upwards ) s = translate x y $ rotate (-90) $ paintStone s
paintLocatedStone ( (x,y), Downwards ) s = translate x y $ rotate 90 $ paintStone s
paintLocatedStone ( (x,y), Leftwards ) s = translate x y $ rotate 180 $ paintStone s
paintLocatedStone ( (x,y), Rightwards ) s = translate x y $ paintStone s

paintHandStone :: Point -> Stone -> Picture
paintHandStone (x,y) s = translate x y $ rotate 90 $ paintStone s

paintPositionedStone :: PositionedStone -> Picture
paintPositionedStone = uncurry paintLocatedStone

paintPositionedSnake :: ViewableSnake -> [Picture]
paintPositionedSnake pss = map paintPositionedStone $ leftWing pss ++ rightWing pss

paintHand :: [Stone] -> Picture
paintHand ss = pictures $ zipWith paintHandStone handStonePositions ss

paintPlayer :: (Eq player, ShowUnquoted player) => player -> Maybe player -> LocatedPlayer player -> Hand player -> Picture
paintPlayer current winner (LocatedPlayer name (x,y) angle) h =
    translate x y  $ rotate angle $ pictures
      [ bgColor background
      , scale 0.5 0.5 $ translate ( charHalfWidth * ( -nameLength ) ) 0 $ fgColor $ text nameString
      , paintHand $ stones h
      ]
      where
          nameString = showWithoutQuotes name
          nameLength = genericLength nameString :: Float
          wins = case winner of
              Nothing -> False
              Just theWinner -> name == fromJust winner
          plays = name == current
          bgColor
            | wins = color white
            | plays = color cyan
            | otherwise = id
          fgColor
            | wins = color red
            | plays = color black
            | otherwise = id
          background = if wins || plays then translate offset (charHalfHeight-offset) $ rectangleSolid (charWidth*nameLength+2*offset) (charHeight+2*offset) else blank
          charHalfWidth = 30 :: Float
          charWidth = 2*charHalfWidth
          charHalfHeight = 30 :: Float
          charHeight = 2*charHalfHeight
          offset = 10 :: Float

paintPlayers :: (Eq player, ShowUnquoted player) => [LocatedPlayer player] -> [Hand player] -> player -> Maybe player -> [Picture]
paintPlayers poss hs current winner = zipWith (paintPlayer current winner) sortedPoss hs
    where
      sortedPoss = map selectPoss hs
      selectPoss h = fromJust $ find (\p -> label p == player h  ) poss

paintState :: (Eq player, ShowUnquoted player) => ViewState player -> Picture
paintState vs = pictures $ color chartreuse ( circleSolid tableRadius ) :
                paintPlayers (players vs) theHands ( player $ head theHands) (winner vs) ++
                paintPositionedSnake ( viewable vs )
                where
                  theState = head $ steps vs
                  theHands = hands theState

type PositionedStone = (Positioned, Stone)

initialViewableSnake :: ViewableSnake
initialViewableSnake = ViewableSnake [] [] 0 0

newPlacement :: GameState player -> ViewableSnake -> ViewableSnake
newPlacement (GameState [] _ _ ) vs = vs
newPlacement gs (ViewableSnake _ _ _ 0) = ViewableSnake [newPos] [] 0 1
  where
    stone = head $ snake gs
    how = if isDouble stone then Upwards else Rightwards
    newPos = (((0,0),how), stone)
newPlacement gs vs@(ViewableSnake ll rl pos len)
  | doNothing = vs
  | gotoRight = ViewableSnake ll (newPos:rl) pos (len+1)
  | otherwise = ViewableSnake (newPos:ll) rl pos (len+1)
  where
    gotoRight = snk !! pos == (snd.head) ll
    doNothing = length snk == len
    snk = snake gs
    stone = ( if gotoRight then last else head ) snk
    otherStone = if gotoRight && (not.null) rl then head rl else head ll
    reference = (fst.fst.fst) otherStone
    (how,movement) = if isDouble stone then (Upwards,heightStone/2) else (Rightwards,widthStone/2)
    otherMovement = if isDouble $ snd otherStone then heightStone/2 else widthStone/2
    x =  if gotoRight then  reference + movement + otherMovement else reference - movement - otherMovement
    newPos = (((x,0),how), stone) :: PositionedStone

advanceState :: viewPort -> Float -> ViewState player -> ViewState player
advanceState _ _ (ViewState ss@[s] positions np _) = ViewState ss (newPlacement s positions) np $ Just $ player $ head $ hands s
advanceState _ _ (ViewState (s:ss) positions np _) = ViewState ss (newPlacement s positions) np Nothing

initViewState :: [GameState player] -> ViewState player
initViewState ss = ViewState ss initialViewableSnake posPlayers Nothing
  where
    players = map player $ hands $ head ss
    numPlayers = genericLength players :: Float
    angleAdvance = 360 / numPlayers
    angles360 = [0,angleAdvance..(360-angleAdvance)]
    angles = map (* (2*pi/360)) angles360
    positions = map (\x->(playerRadius*sin x,playerRadius*cos x)) angles ::[Point]
    posPlayers = map (\(pl,po,an) -> LocatedPlayer pl po an) $ zip3 players positions angles360

showGame :: (Eq player, ShowUnquoted player) => [GameState player] -> IO ()
showGame [] = return ()
showGame ss =
   simulate (InWindow "Domino" (windowSizeX, windowSizeY) (10, 10)) green 4 (initViewState ss) paintState advanceState
