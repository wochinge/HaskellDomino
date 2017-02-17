module View (
  showGame
) where

import Data.List(intercalate)
import Graphics.Gloss
import Play(GameState, snake, hands, skips,Hand, player, stones)



showHand :: Show player => Hand player -> String
showHand hand = "Player " ++ (show $ player hand) ++ " = " ++ show (stones hand)

showGameState :: Show player => GameState player -> [String]
showGameState state =
  ("Snake is " ++ show ( snake state) ) :
  ("Number of skips: " ++ show (skips state)) :
  map showHand (hands state)

showGame :: Show player => [GameState player] -> IO ()
showGame ss = do
   putStrLn $ intercalate "\n" $ concatMap showGameState ( take 100 ss )
   display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
