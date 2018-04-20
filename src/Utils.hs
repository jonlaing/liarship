module Utils where

import GameState
import Data.Maybe

nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

opponent :: State a -> Player
opponent = nextPlayer . currentPlayer

switchPlayer :: State a -> State a
switchPlayer s = s { currentPlayer = nextPlayer $ currentPlayer s }

current :: (State a -> (b, b)) -> State a -> b
current f s = case (currentPlayer s) of
  Player1 -> fst $ f s
  Player2 -> snd $ f s

destroyedShips' :: Board -> Int
destroyedShips' b = length $ filter (== Ship Hit) b

destroyedShips :: State a -> Int
destroyedShips s =
  destroyedShips' board
  where board = current boards s

modifyBoards :: (Board -> Board) -> Player -> State a -> State b
modifyBoards f p s = if (currentPlayer s) == p
  then s { boards = (f $ fst $ boards s, snd $ boards s) }
  else s { boards = (fst $ boards s, f $ snd $ boards s) }

modifyMissles :: (Int -> Int) -> Player -> State a -> State b
modifyMissles f p s = if (currentPlayer s) == p
  then s { missles = (f $ fst $ missles s , snd $ missles s) }
  else s { missles = (fst $ missles s, f $ snd $ missles s) }

coordToIdx :: Coord -> Int
coordToIdx (x, y) = (y * boardWidth) + x

getCoord :: Board -> Coord -> Maybe Tile
getCoord b c =
  if i < length b then Just (b !! i) else Nothing
  where i = coordToIdx c

isShip :: Tile -> Bool
isShip (Ship _) = True
isShip _ = False

shipAtActiveCoord :: State a -> Bool
shipAtActiveCoord s = fromMaybe False $
  (activeCoordinate s)
  >>= getCoord (current boards s)
  >>= Just . isShip

hitShipAtCoord :: Coord -> Board -> Board
hitShipAtCoord c b =
  hd ++ [Ship Hit] ++ tl
  where (hd,_:tl) = splitAt i b
        i = coordToIdx c

hitActiveCoord :: State a -> State b
hitActiveCoord s = 
  case mc of
    Just c -> modifyBoards (hitShipAtCoord c) p s
    Nothing -> s { activeCoordinate = Nothing }
  where mc = activeCoordinate s
        p = currentPlayer s

winByAmmo :: State a -> Maybe WinState
winByAmmo s = case (missles s) of
  (0, _) -> Just $ ByAmmo Player2
  (_, 0) -> Just $ ByAmmo Player1
  (_, _) -> Nothing

winByDestruction :: State a -> Maybe WinState
winByDestruction s =
  if (destroyedShips s) == totalShipSizes
    then Just $ ByDestruction $ opponent s
    else Nothing