module View (
  showGame
) where

import Play(GameState)

showGame :: Show player => [GameState player] -> IO ()
showGame = do
  putStrLn . show
