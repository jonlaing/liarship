module Board where

import System.Random
import Data.List.Index (indexed)

data ShipState = Unhit
               | Hit
               deriving (Eq, Show)

data Tile = Blank
          | Ship ShipState
          deriving (Eq, Show)

type Coord = (Int, Int)
type BoardWidth = Int

data Board =
  Board BoardWidth [Tile]
  deriving (Show)

type ShipLength = Int
data ShipType = Carrier
              | BattleShip
              | Cruiser
              | Submarine
              | Destroyer
              deriving (Eq, Show)

data Orientation = Horizontal
                 | Vertical
                 deriving (Eq, Show)

shipSize :: ShipType -> Int
shipSize Carrier = 5
shipSize BattleShip = 4
shipSize Cruiser = 3
shipSize Submarine = 3
shipSize Destroyer = 2

totalShipSizes :: Int
totalShipSizes =
  foldr (+) 0 $
  map shipSize [ Carrier
               , BattleShip
               , Cruiser
               , Submarine
               , Destroyer ]

coordToIdx :: BoardWidth -> Coord -> Int
coordToIdx w (x, y) = (y * w) + x

idxToCoord :: BoardWidth -> Int -> Coord
idxToCoord w i = (i `mod` (w-1), i `div` (w-1))

possiblePositions :: Board -> Orientation -> ShipLength -> [Coord]
possiblePositions b o l = 
  filter (noOverlap b o l) $ boardToCoords b



boardToCoords :: Board -> [Coord]
boardToCoords (Board w b) = 
  [(x `mod` w, y `div` w) | x <- [0..w-1], y <- [0,w..(w*w)-1]]


noOverlap :: Board -> Orientation -> ShipLength -> Coord -> Bool
noOverlap (Board w b) o l c =
  foldr (\c' a -> a && blankAt c') True $ allShipCoords o l c
  where blankAt c' = let i = coordToIdx w c' in
          if i < length b then b !! i == Blank else False

allShipCoords :: Orientation -> ShipLength -> Coord -> [Coord]
allShipCoords Horizontal l (x, y) = [(x', y) | x' <- [x..(x + l - 1)]]
allShipCoords Vertical l (x, y) = [(x, y') | y' <- [y..(y + l - 1)]]

placeShip :: Board -> Orientation -> ShipLength -> Coord -> Board
placeShip b o l c =
  foldr (\c' b' -> placeTile b' (Ship Unhit) c') b $ allShipCoords o l c

placeTile :: Board -> Tile -> Coord -> Board
placeTile (Board w b) t c =
  Board w $ hd ++ [t] ++ tl
  where (hd,_:tl) = splitAt i b
        i = coordToIdx w c

randomCoord :: Board -> Orientation -> ShipLength -> IO Coord
randomCoord b o l = (coords !!) <$> randomRIO(0, (length coords)-1)
  where coords = possiblePositions b o l
        Board w _ = b

randomOrientation :: IO Orientation
randomOrientation = ([Horizontal, Vertical] !!) <$> randomRIO (0,1)

randomBoard :: Board -> [ShipType] -> IO Board
randomBoard b [] = return b
randomBoard b (s:ss) = do
  rCoord <- randomCoord b Horizontal (shipSize s)
  randomBoard (placeShip b Horizontal (shipSize s) rCoord) ss

randomCoordOf :: Board -> Tile -> IO Coord
randomCoordOf (Board w b) t =
  idxToCoord w <$> (b' !!) <$> randomRIO (0, (length b) - 1)
  where b' = map fst $ filter ((== t) . snd) $ indexed b