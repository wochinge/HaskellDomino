module View (
  showGame
) where

import Data.List(intercalate)
import Graphics.Gloss
import Graphics.Gloss.Data.Picture(Picture(..))
import Play(GameState, snake, hands, skips,Hand, player, stones, DirectedStone, first, second)



showHand :: Show player => Hand player -> String
showHand hand = "Player " ++ (show $ player hand) ++ " = " ++ show (stones hand)

showGameState :: Show player => GameState player -> [String]
showGameState state =
  ("Snake is " ++ show ( snake state) ) :
  ("Number of skips: " ++ show (skips state)) :
  map showHand (hands state)

data ViewState player = ViewState
    { steps :: [GameState player]
    }

oneDot :: Picture
oneDot = color black $ circleSolid 3


dotPositions = [-4,4,0]

paintDots :: Int -> [Picture]
paintDots n
    | n == 0 = []
    | odd n = oneDot : (paintDots (n-1))
    | otherwise = [rotate 180 halfDots, halfDots]
    where halfDots = pictures $ map (\x -> translate x 6 oneDot ) $ take (n `div` 2) dotPositions

paintStone :: DirectedStone -> Picture
paintStone ds = pictures $
    [ color white $ rectangleSolid 41 21
    , color black $ rectangleWire 41 21
    , color black $ line [(0,-10),(0,10)]
    , color orange $ circleSolid 4
    , color black $ circle 4
    ] ++ (map (translate (-10) 0) (paintDots $ first ds)) ++ (map (translate 10 0) (paintDots $ second ds))

data Positioning = Upwards | Downwards | Leftwards | Rightwards
paintLocatedStone

paintState :: ViewState player -> Picture
paintState vs = if null (snake $ head $ steps vs) then blank else paintStone $ head $ snake $ head $ steps vs

advanceState :: viewPort -> Float -> ViewState player -> ViewState player
advanceState _ _ vs@(ViewState []) =  vs
advanceState _ _ (ViewState (s:ss)) = ViewState ss

showGame :: Show player => [GameState player] -> IO ()
showGame ss = do
   putStrLn $ intercalate "\n" $ concatMap showGameState ( take 100 ss )
   display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
