module Human where

import GameState ( activeCoordinate,
                   currentPlayer,
                   State,
                   Fire,
                   Defend,
                   EndGame,
                   HitStatus(DirectHit, Miss, Tricked, Deflect))
import Board (Coord)
import qualified Reducers as R
import Utils (opponentPlayer)
import Print (green, red, clearChar)

fire :: State Fire -> IO (State Defend)
fire s = do
  putStrLn $
    (show $ currentPlayer s) ++
    ", enter coodinates to fire on:"
  coords <- (\s -> read s :: Coord) <$> getLine
  return $ R.fire coords s


checkDefend :: State Defend -> IO (State EndGame)
checkDefend s = do
  putStrLn $
    (show $ currentPlayer s) ++
    ", do you want to defend? (y/n)"
  char <- getLine
  case char of
    "y" -> defend s
    "n" -> nodefend s
    _ -> do
      putStrLn "Please type either 'y' or 'n'"
      defend s


defend :: State Defend -> IO (State EndGame)
defend s = case R.defend s of
  Deflect s' -> do
    putStrLn $ green ++ (show $ currentPlayer s') ++ " Deflected the missle!" ++ clearChar
    return s'
  Tricked s' -> do
    putStrLn $ red ++ (show $ currentPlayer s') ++ " wasted a missle!" ++ clearChar
    return s'
  m -> do
    putStrLn $ "Something unexpected happened..." ++ (show m)
    return $ s {activeCoordinate = Nothing}
  

nodefend :: State Defend -> IO (State EndGame)
nodefend s = case R.nodefend s of
  DirectHit s' -> do
    putStrLn $ green ++ (show $ opponentPlayer s') ++ " made a Direct Hit!" ++ clearChar
    return s'
  Miss s' -> do
    putStrLn $ red ++ (show $ opponentPlayer s') ++ " Missed!" ++ clearChar
    return s'
  m -> do
    putStrLn $ "Something unexpected happened..." ++ (show m)
    return $ s {activeCoordinate = Nothing}