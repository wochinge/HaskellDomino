module View
( showGame
) where

import Data.List(intercalate, genericLength, find)
import Data.Maybe(fromJust, isJust)
import Graf
import Play
    ( GameState(..)
    , Hand(..)
    , Stone(..)
    , Snake
    , isDouble
    )
import VisibleStone( paintStone )
import NewPlacement
    ( newPlacement
    , ViewableSnake(..)
    , initialViewableSnake
    , Positioning(..)
    , Positioned
    , PositionedStone
    )
import Debug.Hood.Observe
import Generics.Deriving
import Control.DeepSeq

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
    } deriving (Show, Generic)
instance Observable player => Observable (LocatedPlayer player)

data ViewState player = ViewState
    { steps :: [GameState player]
    , viewable :: ViewableSnake
    , players :: [LocatedPlayer player]
    , winner :: Maybe player
    } deriving (Show, Generic)
instance Observable player => Observable (ViewState player)

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
paintPositionedSnake pss
    | isJust central = map paintPositionedStone $ fromJust central : leftWing pss ++ rightWing pss
    | otherwise = []
    where
        central = center pss

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

advanceState :: (Show player, Observable player) => ViewState player -> ViewState player
advanceState (ViewState ss@[s] positions np _) = ViewState ss (newPlacement tableRadius s positions) np $ Just $ player $ head $ hands s
advanceState (ViewState (s:ss) positions np _) = ViewState ss (newPlacement tableRadius s positions) np Nothing

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

showGame :: (Observable player, Eq player, ShowUnquoted player, Show player) => [GameState player] -> IO ()
showGame [] = return ()
showGame ss =
    -- simulate (InWindow "Domino" (windowSizeX, windowSizeY) (10, 10)) green 4 (initViewState ss) paintState advanceState
    let
        stateList = initViewState ss : map advanceState stateList
        theLast = (fromJust . find (isJust.winner)) stateList
    in putStrLn $ "steps=" ++ (show.steps) theLast ++ "\n\nviewable="++ (show.viewable) theLast
