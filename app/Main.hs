module Main where

import Control.Monad (forever)
import GameState
import Utils
import qualified Reducers as R
import System.Exit (exitSuccess)

printState :: State a -> IO (State a)
printState s = do
  putStrLn $ show s
  return s

printPlayerStat :: String -> Int -> Int -> String
printPlayerStat player missles damaged =
  player ++ " |" ++
  " Missles: " ++ (show missles) ++
  " Damage: " ++ (show damaged) ++ "/" ++ (show totalShipSizes)


printStats :: State a -> IO (State a)
printStats s = do
  putStrLn $ printPlayerStat "Player 1" m1 d1
  putStrLn $ printPlayerStat "Player 2" m2 d2
  return s
  where m1 = fst $ missles s
        d1 = destroyedShips' $ fst $ boards s
        m2 = snd $ missles s
        d2 = destroyedShips' $ snd $ boards s


startGame :: State NewGame -> IO (State Fire)
startGame s = do
  putStrLn "Game Loop"
  return $ s {winner = Nothing}

fire :: State Fire -> IO (State Defend)
fire s = do
  putStrLn $
    (show $ currentPlayer s) ++
    ", enter coodinates to fire on:"
  coords <- (\s -> read s :: Coord) <$> getLine
  return $ R.fire coords s

defend :: State Defend -> IO (State EndGame)
defend s = do
  putStrLn $
    (show $ currentPlayer s) ++
    ", do you want to defend? (y/n)"
  char <- getLine
  case char of
    "y" -> defend' s
    "n" -> nodefend' s
    _ -> do
      putStrLn "Please type either 'y' or 'n'"
      defend' s

defend' :: State Defend -> IO (State EndGame)
defend' s = case R.defend s of
  Deflect s' -> do
    putStrLn "Deflected!"
    return s'
  Tricked s' -> do
    putStrLn "You've wasted a missle!"
    return s'
  m -> do
    putStrLn $ "Something unexpected happened..." ++ (show m)
    return $ s {activeCoordinate = Nothing}
  
nodefend' :: State Defend -> IO (State EndGame)
nodefend' s = case R.nodefend s of
  DirectHit s' -> do
    putStrLn "Direct Hit!"
    return s'
  Miss s' -> do
    putStrLn "Missed!"
    return s'
  m -> do
    putStrLn $ "Something unexpected happened..." ++ (show m)
    return $ s {activeCoordinate = Nothing}

checkWinner :: State EndGame -> IO (Either WinState (State NewGame))
checkWinner s = return $ R.endGame s 


gameLoop :: State NewGame -> IO (State NewGame)
gameLoop init = forever $ do
  result <- startGame init
    >>= fire
    >>= defend
    >>= printStats
    >>= checkWinner

  case result of
    Left w -> do
      putStrLn $ show w
      exitSuccess
    Right s -> gameLoop s

board :: Board
board =
  (take 5 $ repeat $ Ship Unhit ) ++ (take 5 $ repeat Blank) ++
  (take 5 $ repeat $ Ship Unhit ) ++ (take 5 $ repeat Blank) ++
  (take 4 $ repeat $ Ship Unhit ) ++ (take 6 $ repeat Blank) ++
  (take 2 $ repeat $ Ship Unhit ) ++ (take 8 $ repeat Blank) ++
  (take 1 $ repeat $ Ship Unhit ) ++ (take 9 $ repeat Blank)

initialState :: State NewGame
initialState = State { activeCoordinate = Nothing
                     , boards = (board, board)
                     , currentPlayer = Player1
                     , missles = (10, 10)
                     , winner = Nothing
                     }

main :: IO (State NewGame)
main = gameLoop initialState
