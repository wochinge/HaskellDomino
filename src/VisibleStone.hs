module VisibleStone

where
import Graphics.Gloss
import Play ( Stone(..) )

oneDot :: Picture
oneDot = color black $ circleSolid 3

dotPositions :: [Float]
dotPositions = [-6,6,0]

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
