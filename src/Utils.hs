module Utils where

import GameState
import Board
import Data.Maybe

nextPlayer :: Player -> Player
nextPlayer Human = Computer
nextPlayer Computer = Human

opponentPlayer :: State a -> Player
opponentPlayer = nextPlayer . currentPlayer

switchPlayer :: State a -> State a
switchPlayer s = s { currentPlayer = nextPlayer $ currentPlayer s }

current :: (State a -> (b, b)) -> State a -> b
current f s = case (currentPlayer s) of
  Human -> fst $ f s
  Computer -> snd $ f s

opponent :: (State a -> (b, b)) -> State a -> b
opponent f s = case (currentPlayer s) of
  Computer -> fst $ f s
  Human -> snd $ f s

destroyedShips' :: Board -> Int
destroyedShips' (Board _ b) = length $ filter (== Ship Hit) b

destroyedShips :: State a -> Int
destroyedShips s =
  destroyedShips' board
  where board = current boards s

modifyBoards :: (Board -> Board) -> Player -> State a -> State b
modifyBoards f Human s = s { boards = (f $ fst $ boards s, snd $ boards s) }
modifyBoards f Computer s = s { boards = (fst $ boards s, f $ snd $ boards s) }

modifyMissles :: (Int -> Int) -> Player -> State a -> State b
modifyMissles f Human s = s { missles = (f $ fst $ missles s , snd $ missles s) }
modifyMissles f Computer s = s { missles = (fst $ missles s, f $ snd $ missles s) }


getCoord :: Board -> Coord -> Maybe Tile
getCoord (Board w b) c =
  if i < length b then Just (b !! i) else Nothing
  where i = coordToIdx w c

isShip :: Tile -> Bool
isShip (Ship _) = True
isShip _ = False

shipAtActiveCoord :: State a -> Bool
shipAtActiveCoord s = fromMaybe False $
  (activeCoordinate s)
  >>= getCoord (current boards s)
  >>= Just . isShip

hitShipAtCoord :: Coord -> Board -> Board
hitShipAtCoord c (Board w b) =
  Board w $ hd ++ [Ship Hit] ++ tl
  where (hd,_:tl) = splitAt i b
        i = coordToIdx w c

hitActiveCoord :: State a -> State b
hitActiveCoord s = 
  case mc of
    Just c -> modifyBoards (hitShipAtCoord c) p s
    Nothing -> s { activeCoordinate = Nothing }
  where mc = activeCoordinate s
        p = currentPlayer s

winByAmmo :: State a -> Maybe WinState
winByAmmo s = case (missles s) of
  (0, _) -> Just $ ByAmmo Computer
  (_, 0) -> Just $ ByAmmo Human
  (_, _) -> Nothing

winByDestruction :: State a -> Maybe WinState
winByDestruction s =
  if (destroyedShips s) == totalShipSizes
    then Just $ ByDestruction $ opponentPlayer s
    else Nothing