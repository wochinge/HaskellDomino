module VisibleStone

where
import Graphics.Gloss
import Play ( Stone(..) )


widthStone :: Float
widthStone = 40

heightStone :: Float
heightStone = 20

radiusDot :: Float
radiusDot = 3

radiusButton :: Float
radiusButton = 3

innerRadiusButton = radiusButton - 1

oneDot :: Picture
oneDot = color black $ circleSolid radiusDot

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
    [ color white $ rectangleSolid widthStone heightStone
    , color black $ rectangleWire widthStone heightStone
    , translate 0 moveLine $ color black $ line [(0,0),(0,heightStone)]
    , color orange $ circleSolid radiusButton
    , color black $ circle innerRadiusButton
    ] ++ map (translate (-widthStone) 0) (paintDots $ first ds) ++ map (translate widthStone 0) (paintDots $ second ds)
    where
        moveLine = heightStone / 2
        moveDots = widthStone / 4
