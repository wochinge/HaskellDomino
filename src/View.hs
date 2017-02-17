module View (
  showGame
) where

import Data.List(intercalate)
import Graphics.Gloss
import Graphics.Gloss.Data.Picture(Picture(..))
import Play
    ( GameState
    , snake
    , hands
    , skips
    , Hand
    , player
    , stones
    , DirectedStone
    , first
    , second
    , Snake
    )



showHand :: Show player => Hand player -> String
showHand hand = "Player " ++ show (player hand) ++ " = " ++ show (stones hand)

showGameState :: Show player => GameState player -> [String]
showGameState state =
  ("Snake is " ++ show ( snake state) ) :
  ("Number of skips: " ++ show (skips state)) :
  map showHand (hands state)

newtype ViewState player = ViewState
    { steps :: [GameState player]
    }

oneDot :: Picture
oneDot = color black $ circleSolid 3


data Positioning = Upwards | Downwards | Leftwards | Rightwards deriving Show

-- Constants that are adequate only for 0..6 stone set
dotPositions :: [Float]
dotPositions = [-6,6,0]

-- Initial solution to paint, shitty
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

-- End of specific constants

paintDots :: Int -> [Picture]
paintDots n
    | n == 0 = []
    | odd n = oneDot : paintDots (n-1)
    | otherwise = [rotate 180 halfDots, halfDots]
    where halfDots = pictures $ map (\x -> translate x 6 oneDot ) $ take (n `div` 2) dotPositions

paintStone :: DirectedStone -> Picture
paintStone ds = pictures $
    [ color white $ rectangleSolid 41 21
    , color black $ rectangleWire 41 21
    , color black $ line [(0,-10),(0,10)]
    , color orange $ circleSolid 3
    , color black $ circle 2
    ] ++ map (translate (-10) 0) (paintDots $ first ds) ++ map (translate 10 0) (paintDots $ second ds)

paintLocatedStone :: Positioned -> DirectedStone -> Picture
paintLocatedStone ( (x,y), Upwards ) s = translate x y $ rotate (-90) $ paintStone s
paintLocatedStone ( (x,y), Downwards ) s = translate x y $ rotate 90 $ paintStone s
paintLocatedStone ( (x,y), Leftwards ) s = translate x y $ rotate 180 $ paintStone s
paintLocatedStone ( (x,y), Rightwards ) s = translate x y $ paintStone s

paintSnake :: Snake-> [Picture]
paintSnake = zipWith paintLocatedStone  positions

paintState :: ViewState player -> Picture
paintState vs = pictures $  paintSnake $ snake $ head $ steps vs


advanceState :: viewPort -> Float -> ViewState player -> ViewState player
advanceState _ _ (ViewState ss@[s]) =  ViewState ss
advanceState _ _ (ViewState (s:ss)) = ViewState ss

showGame :: Show player => [GameState player] -> IO ()
showGame ss = do
   putStrLn $ intercalate "\n" $ concatMap showGameState ( take 100 ss )
   simulate (InWindow "Nice Window" (800, 800) (10, 10)) green 4 (ViewState ss) paintState advanceState
