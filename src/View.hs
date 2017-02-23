module View (
  showGame
) where

import Data.List(intercalate, genericLength, find)
import Data.Maybe(fromJust)
import Graphics.Gloss
import Graphics.Gloss.Data.Picture(Picture(..))
import Play
    ( GameState(..)
    , Hand(..)
    , Stone(..)
    , Snake
    , Hand
    )

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

data ViewState player = ViewState
    { steps :: [GameState player]
    , players :: [LocatedPlayer player]
    , lastHand :: Bool
    }

oneDot :: Picture
oneDot = color black $ circleSolid 3


data Positioning = Upwards | Downwards | Leftwards | Rightwards deriving Show

-- Constants that are adequate only for 0..6 stone set
dotPositions :: [Float]
dotPositions = [-6,6,0]

-- y coordinates
upperLevel,mediumLevel,lowerLevel :: Float
(upperLevel, mediumLevel, lowerLevel) = (100,0,-100)
ascending1,ascending2,ascending3 :: Float
(ascending1, ascending2, ascending3) = (10,50,90)
descending1,descending2,descending3 :: Float
(descending1, descending2, descending3) = (-10,-50,-90)
-- x coordinates
ascending,descending :: Float
(ascending, descending) = (-190,190)
xPositions :: [Float]
xPositions = [-160,-120..160]

-- stone coordinates for simple layout
type Positioned = (Point,Positioning)
positions :: [Positioned]
positions =
    [ ((xPositions !! 5, upperLevel),Leftwards)
    , ((xPositions !! 4, upperLevel),Leftwards)
    , ((xPositions !! 3, upperLevel),Leftwards)
    , ((xPositions !! 2, upperLevel),Leftwards)
    , ((xPositions !! 1, upperLevel),Leftwards)
    , ((head xPositions, upperLevel),Leftwards)

    , ((ascending, ascending3),Downwards)
    , ((ascending, ascending2),Downwards)
    , ((ascending, ascending1),Downwards)

    , ((head xPositions, mediumLevel),Rightwards)
    , ((xPositions !! 1, mediumLevel),Rightwards)
    , ((xPositions !! 2, mediumLevel),Rightwards)
    , ((xPositions !! 3, mediumLevel),Rightwards)
    , ((xPositions !! 4, mediumLevel),Rightwards)
    , ((xPositions !! 5, mediumLevel),Rightwards)
    , ((xPositions !! 6, mediumLevel),Rightwards)
    , ((xPositions !! 7, mediumLevel),Rightwards)
    , ((xPositions !! 8, mediumLevel),Rightwards)

    , ((descending, descending1),Downwards)
    , ((descending, descending2),Downwards)
    , ((descending, descending3),Downwards)

    , ((xPositions !! 8, lowerLevel),Leftwards)
    , ((xPositions !! 7, lowerLevel),Leftwards)
    , ((xPositions !! 6, lowerLevel),Leftwards)
    , ((xPositions !! 5, lowerLevel),Leftwards)
    , ((xPositions !! 4, lowerLevel),Leftwards)
    , ((xPositions !! 3, lowerLevel),Leftwards)
    , ((xPositions !! 2, lowerLevel),Leftwards)
    ]

handStonePositions :: [Point]
handStonePositions =zip posX $ repeat (-50)
  where posX = 0 : concatMap (\x -> [-x,x]) [25,50..]

paintDots :: Int -> [Picture]
paintDots n
    | n == 0 = []
    | odd n = oneDot : paintDots (n-1)
    | otherwise = [rotate 180 halfDots, halfDots]
    where halfDots = pictures $ map (\x -> translate x 6 oneDot ) $ take (n `div` 2) dotPositions

paintStone :: Stone -> Picture
paintStone ds = pictures $
    [ color white $ rectangleSolid 41 21
    , color black $ rectangleWire 41 21
    , color black $ line [(0,-10),(0,10)]
    , color orange $ circleSolid 3
    , color black $ circle 2
    ] ++ map (translate (-10) 0) (paintDots $ first ds) ++ map (translate 10 0) (paintDots $ second ds)

paintLocatedStone :: Positioned -> Stone -> Picture
paintLocatedStone ( (x,y), Upwards ) s = translate x y $ rotate (-90) $ paintStone s
paintLocatedStone ( (x,y), Downwards ) s = translate x y $ rotate 90 $ paintStone s
paintLocatedStone ( (x,y), Leftwards ) s = translate x y $ rotate 180 $ paintStone s
paintLocatedStone ( (x,y), Rightwards ) s = translate x y $ paintStone s

paintHandStone :: Point -> Stone -> Picture
paintHandStone (x,y) s = translate x y $ rotate 90 $ paintStone s

paintSnake :: Snake-> [Picture]
paintSnake = zipWith paintLocatedStone  positions

paintHand :: [Stone] -> Picture
paintHand ss = pictures $ zipWith paintHandStone handStonePositions ss

paintPlayer :: ShowUnquoted player => LocatedPlayer player -> Hand player -> Picture
paintPlayer (LocatedPlayer name (x,y) angle) h =
    translate x y  $ rotate angle $ pictures
      [ scale 0.5 0.5 $ text $ showWithoutQuotes name
      , paintHand $ stones h
      ]

paintPlayers :: (Eq player, ShowUnquoted player) => [LocatedPlayer player] -> [Hand player] -> [Picture]
paintPlayers poss hs = zipWith paintPlayer sortedPoss hs
    where
      sortedPoss = map selectPoss hs
      selectPoss h = fromJust $ find (\p -> label p == player h  ) poss

paintWinner :: ShowUnquoted player => [LocatedPlayer player] -> player -> [Picture]
paintWinner _ p  = [translate (-100) 20 $ scale 0.25 0.25 $ text $ "The winner is " ++ showWithoutQuotes p]

paintState :: (Eq player, ShowUnquoted player) => ViewState player -> Picture
paintState vs = pictures $ paintSnake ( snake theState) ++
                paintPlayers (players vs) ( hands theState ) ++
                theWinner
                where
                  theState = head $ steps vs
                  theWinner =  if lastHand vs
                               then paintWinner (players vs) (player $ head $ hands theState)
                               else [blank]

advanceState :: viewPort -> Float -> ViewState player -> ViewState player
advanceState _ _ (ViewState ss@[s] np _) =  ViewState ss np True -- Missing 'the winner is'
advanceState _ _ (ViewState (s:ss) np _) = ViewState ss np False

initViewState :: [GameState player] -> ViewState player
initViewState ss = ViewState ss posPlayers False
  where
    players = map player $ hands $ head ss
    numPlayers = genericLength players :: Float
    angleAdvance = 360 / numPlayers
    angles360 = [0,angleAdvance..(360-angleAdvance)]
    angles = map (* (2*pi/360)) angles360
    radiusx = 280:: Float
    radiusy = 250 :: Float
    positions = map (\x->(radiusx*sin x,radiusy*cos x)) angles ::[Point]
    posPlayers = map (\(pl,po,an) -> LocatedPlayer pl po an) $ zip3 players positions angles360

showGame :: (Eq player, ShowUnquoted player) => [GameState player] -> IO ()
showGame [] = return ()
showGame ss =
   simulate (InWindow "Domino" (800, 800) (10, 10)) green 4 (initViewState ss) paintState advanceState
