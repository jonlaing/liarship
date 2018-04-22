{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module GameState where

import Data.Maybe
import Board


data NewGame
data Fire
data Defend
data CheckWinner
data EndGame


data Player = Human
            | Computer
            deriving (Eq, Show)

data WinState = ByAmmo Player
              | ByDestruction Player
              deriving (Show)


data HitStatus = DirectHit (State EndGame)
               | Deflect (State EndGame)
               | Miss (State EndGame)
               | Tricked (State EndGame)
               deriving (Show)

data Difficulty = Beginner
                | Easy
                | Medium
                | Hard
                | Expert
                deriving (Show)


data State ph = State { activeCoordinate :: Maybe Coord
                      , boards :: (Board, Board)
                      , currentPlayer :: Player
                      , missles :: (Int, Int)
                      , winner :: Maybe Player
                      , difficulty :: Difficulty
                      } deriving (Show)