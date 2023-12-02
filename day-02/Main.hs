module Main where

import System.IO
import Data.List.Split

getNumFromCol :: String -> String -> (Int, Int, Int)
getNumFromCol nb "red" = (read nb, 0, 0)
getNumFromCol nb "green" = (0, read nb, 0)
getNumFromCol nb "blue" = (0, 0, read nb)

maxOfSeen :: [String] -> [[Int]]
maxOfSeen [] = [[], [], []]
maxOfSeen (nb : color : list) = [red : tred, green : tgreen, blue : tblue]
            where [tred, tgreen, tblue] = maxOfSeen list
                  (red, green, blue) = getNumFromCol nb color

main = do
    file <- readFile "input.txt"
    putStrLn "Exo 1:"
    let games = map (map (filter (/= ',')) . concatMap (filter (\s -> s /= "" && s /= "Game" && last s /= ':') . splitOn " ") . splitOn ";") $ lines file
    let maximums = map (map maximum . maxOfSeen) games
    let maximumPlays = zip [1..] maximums
    print $ sum $ map fst $ filter (\(id, [red, green, blue]) -> red <= 12 && green <= 13 && blue <= 14) maximumPlays

    putStrLn "Exo 2:"
    print $ sum $ map product maximums
