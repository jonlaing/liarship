module Main where

import Control.Monad (forever)
import GameState
import Utils
import qualified Reducers as R
import System.Exit (exitSuccess)
import Board
import AI

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
  return $ s {winner = Nothing}

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
    putStrLn $ (show $ currentPlayer s') ++ " Deflected the missle!"
    return s'
  Tricked s' -> do
    putStrLn $ (show $ currentPlayer s') ++ " wasted a missle!"
    return s'
  m -> do
    putStrLn $ "Something unexpected happened..." ++ (show m)
    return $ s {activeCoordinate = Nothing}
  
nodefend :: State Defend -> IO (State EndGame)
nodefend s = case R.nodefend s of
  DirectHit s' -> do
    putStrLn $ (show $ opponentPlayer s') ++ " made a Direct Hit!"
    return s'
  Miss s' -> do
    putStrLn $ (show $ opponentPlayer s') ++ " Missed!"
    return s'
  m -> do
    putStrLn $ "Something unexpected happened..." ++ (show m)
    return $ s {activeCoordinate = Nothing}

checkWinner :: State EndGame -> IO (Either WinState (State NewGame))
checkWinner s = return $ R.endGame s 


gameLoop1 :: State NewGame -> IO (State NewGame)
gameLoop1 init = forever $ do
  result <- startGame init
    >>= fire
    >>= checkDefendAI defend nodefend
    >>= printStats
    >>= checkWinner

  case result of
    Left w -> do
      putStrLn $ show w
      exitSuccess
    Right s -> gameLoop2 s

gameLoop2 :: State NewGame -> IO (State NewGame)
gameLoop2 init = forever $ do
  result <- startGame init
    >>= fireAI
    >>= checkDefend
    >>= printStats
    >>= checkWinner

  case result of
    Left w -> do
      putStrLn $ show w
      exitSuccess
    Right s -> gameLoop1 s

board :: Board
board = Board 10 $ take 100 $ repeat Blank

allShips :: [ShipType]
allShips = [Carrier , BattleShip , Cruiser , Submarine , Destroyer]

initialState :: Difficulty -> Board -> Board -> State NewGame
initialState d b1 b2= State { activeCoordinate = Nothing
                          , boards = (b1, b2)
                          , currentPlayer = Human
                          , missles = (10, 10)
                          , winner = Nothing
                          , difficulty = d
                          }

main :: IO (State NewGame)
main = do
  board1 <- randomBoard board allShips
  board2 <- randomBoard board allShips
  gameLoop1 $ initialState Expert board1 board2
