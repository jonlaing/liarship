module Main where

import Control.Monad (forever)
import GameState
import Utils
import qualified Reducers as R
import System.Exit (exitSuccess)
import Board
import AI
import Print


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
  printBoard $ snd $ boards s
  putStrLn $ printPlayerStat "Human   " m1 d1
  putStrLn $ printPlayerStat "Computer" m2 d2
  putStrLn "\n\n"
  return s
  where m1 = fst $ missles s
        d1 = destroyedShips' $ fst $ boards s
        m2 = snd $ missles s
        d2 = destroyedShips' $ snd $ boards s


startGame :: State NewGame -> IO (State Fire)
startGame s = do
  printStats s
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

checkWinner :: State EndGame -> IO (State NewGame)
checkWinner s = do
  case R.endGame s of
    Left w -> do
      putStrLn $ show w
      exitSuccess
    Right s -> return s


gameLoop :: State NewGame -> IO (State NewGame)
gameLoop init = forever $ do
  result <- startGame init
    >>= fire
    >>= checkDefendAI defend nodefend
    >>= checkWinner
    >>= startGame
    >>= fireAI
    >>= checkDefend
    >>= checkWinner

  gameLoop result


board :: Board
board = Board 10 $ take 100 $ repeat Blank

allShips :: [ShipType]
allShips = [Carrier , BattleShip , Cruiser , Submarine , Destroyer]

initialState :: Difficulty -> Board -> Board -> State NewGame
initialState d b1 b2= State { activeCoordinate = Nothing
                          , boards = (b1, b2)
                          , currentPlayer = Human
                          , missles = (50, 50)
                          , winner = Nothing
                          , difficulty = d
                          }

selectDifficulty :: IO (Difficulty)
selectDifficulty = do
  putStrLn "What difficulty level would you like?"
  putStrLn "0 - Beginner   1 - Easy   2 - Medium   3 - Hard   4 - Expert"
  i <- (\s -> read s :: Int) <$> getLine
  case i of
    0 -> return Beginner
    1 -> return Easy
    2 -> return Medium
    3 -> return Hard
    4 -> return Expert
    _ -> do
      putStrLn $ red ++ "That's not an option!" ++ clearChar
      selectDifficulty
                        
main :: IO (State NewGame)
main = do
  board1 <- randomBoard board allShips
  board2 <- randomBoard board allShips
  putStrLn "Welcome to Liarship!"
  difficulty <- selectDifficulty
  gameLoop $ initialState difficulty board1 board2
