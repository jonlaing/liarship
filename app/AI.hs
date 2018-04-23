module AI (fire, checkDefend) where

import Board ( Board(Board)
             , Tile(Blank, Ship)
             , coordToIdx
             , randomCoordOf
             , ShipState(Unhit))
import GameState
import qualified Reducers as R
import System.Random (randomRIO)
import Utils
import System.Exit (exitSuccess)


hitOrBluffRate :: Difficulty -> Int
hitOrBluffRate Beginner = 9
hitOrBluffRate Easy = 8
hitOrBluffRate Medium = 7
hitOrBluffRate Hard = 6
hitOrBluffRate Expert = 5

defendOrNotRate :: Difficulty -> Int
defendOrNotRate Beginner = 1
defendOrNotRate Easy = 2
defendOrNotRate Medium = 3
defendOrNotRate Hard = 4
defendOrNotRate Expert = 5


hitOrBluff :: Difficulty -> Int -> Tile
hitOrBluff d i = if i < (hitOrBluffRate d) then Ship Unhit else Blank

shouldDefend :: Difficulty -> Int -> Bool
shouldDefend d i = i < (defendOrNotRate d)

fallForBluff :: Difficulty -> Int -> Bool
fallForBluff d i = i < (hitOrBluffRate d)

fire :: State Fire -> IO (State Defend)
fire s = do
  rand <- randomRIO (0, 10)
  coord <- randomCoordOf (opponent boards s) (hitOrBluff (difficulty s) rand)
  putStrLn $
    (show $ currentPlayer s) ++
    " is firing at " ++
    (show $ coord)
  return $ R.fire coord s

checkDefend :: ((State Defend) -> IO (State EndGame)) -> ((State Defend) -> IO (State EndGame)) -> State Defend -> IO (State EndGame)
checkDefend defend nodefend s = do
  rand <- randomRIO (0, 10)
  let (Board w b) = current boards s
      d = difficulty s
      c = activeCoordinate s in
        case (b !!) <$> (coordToIdx w <$> c) of
          Just Blank -> do
            if fallForBluff d rand
              then defend s
              else nodefend s
          Just _ -> do
            if shouldDefend d rand
              then defend s
              else nodefend s
          _ -> do
            putStrLn "Something went wrong!"
            exitSuccess
