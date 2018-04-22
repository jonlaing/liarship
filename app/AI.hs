module AI where

import Board
import GameState
import Reducers
import System.Random
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

fireAI :: State Fire -> IO (State Defend)
fireAI s = do
  rand <- randomRIO (0, 10)
  coord <- randomCoordOf (opponent boards s) (hitOrBluff (difficulty s) rand)
  putStrLn $
    (show $ currentPlayer s) ++
    "is firing at " ++
    (show $ coord)
  return $ fire coord s

checkDefendAI :: ((State Defend) -> IO (State EndGame)) -> ((State Defend) -> IO (State EndGame)) -> State Defend -> IO (State EndGame)
checkDefendAI defend nodefend s = do
  rand <- randomRIO (0, 10)
  let (Board w b) = current boards s
      d = difficulty s
      c = activeCoordinate s in
        case (b !!) <$> (coordToIdx w <$> c) of
          Just Blank ->
            if fallForBluff d rand
              then defend s
              else nodefend s
          Just _ ->
            if shouldDefend d rand
              then defend s
              else nodefend s
          _ -> do
            putStrLn "Something went wrong!"
            exitSuccess
