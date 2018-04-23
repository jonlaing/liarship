module Print where

import Board

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