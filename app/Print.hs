module Print where

import Board ( Board(Board)
             , Tile(Blank, Ship)
             , ShipState(Hit, Unhit)
             , BoardWidth
             , totalShipSizes)
import GameState
import Utils (destroyedShips')

escChar :: String
escChar = "\x1b["

clearChar :: String
clearChar = escChar ++ "0m"

red :: String
red = escChar ++ "31m"

green :: String
green = escChar ++ "32m"

showTile :: Tile -> String
showTile Blank = escChar ++ "34m♒︎" ++ clearChar
showTile (Ship Unhit) = "█"
showTile (Ship Hit) = red ++ "︎✸" ++ clearChar

boardLines :: BoardWidth -> [Tile] -> [[Tile]] -> [[Tile]]
boardLines _ [] acc = acc
boardLines w b acc = boardLines w b' acc'
  where b' = drop w b
        acc' = acc ++ [take w b]

printBoard :: Board -> IO ()
printBoard (Board w b) = do
  putStrLn $ "\n\n"
  putStrLn $ "  " ++ unwords nums
  putStrLn $ unlines $ linesWithNums
  where lines = map unwords $ (map . map) showTile $ boardLines w b []
        nums = map show [0..(w-1)]
        linesWithNums = map (\(n, b') -> n ++ " " ++ b') $ zip nums lines


printState :: State a -> IO (State a)
printState s = do
  putStrLn $ show s
  return s

printPlayerStat :: String -> Int -> Int -> Int -> String
printPlayerStat player missles damaged total =
  player ++ " |" ++
  " Missles: " ++ (show missles) ++
  " Damage: " ++ (show damaged) ++ "/" ++ (show total)


printStats :: State a -> IO (State a)
printStats s = do
  printBoard $ snd $ boards s
  putStrLn $ printPlayerStat "Human   " m1 d1 t1
  putStrLn $ printPlayerStat "Computer" m2 d2 t2
  putStrLn "\n\n"
  return s
  where m1 = fst $ missles s
        d1 = destroyedShips' $ fst $ boards s
        t1 = totalShipSizes $ fst $ boards s
        m2 = snd $ missles s
        d2 = destroyedShips' $ snd $ boards s
        t2 = totalShipSizes $ snd $ boards s