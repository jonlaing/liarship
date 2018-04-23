module Main where

import Control.Monad (forever)
import GameState
import Utils (destroyedShips')
import qualified Reducers as R
import System.Exit (exitSuccess)
import Board ( Board(Board)
             , ShipType(Carrier, BattleShip, Cruiser, Submarine, Destroyer)
             , Tile(Blank, Ship)
             , randomBoard
             , totalShipSizes)
import qualified AI as AI
import Print (red, clearChar, printBoard, printStats)
import Human (fire, defend, nodefend, checkDefend)


startGame :: State NewGame -> IO (State Fire)
startGame s = do
  printStats s
  return $ s {winner = Nothing}


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
    >>= AI.checkDefend defend nodefend
    >>= checkWinner
    >>= startGame
    >>= AI.fire
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
