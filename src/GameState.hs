{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module GameState where

import Data.Maybe

boardWidth :: Int
boardWidth = 10


data NewGame
data Fire
data Defend
data CheckWinner
data EndGame


data ShipState = Unhit
               | Hit
               deriving (Eq, Show)

data Tile = Blank
          | Ship ShipState
          deriving (Eq, Show)

data Player = Player1
            | Player2
            deriving (Eq, Show)

data WinState = ByAmmo Player
              | ByDestruction Player
              deriving (Show)

data ShipType = Carrier
              | BattleShip
              | Cruiser
              | Submarine
              | Destroyer
              deriving (Eq, Show)


type Coord = (Int, Int)
type Board = [Tile]


data State ph = State { activeCoordinate :: Maybe Coord
                      , boards :: (Board, Board)
                      , currentPlayer :: Player
                      , missles :: (Int, Int)
                      , winner :: Maybe Player
                      } deriving (Show)

data HitStatus = DirectHit (State EndGame)
               | Deflect (State EndGame)
               | Miss (State EndGame)
               | Tricked (State EndGame)
               deriving (Show)



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
