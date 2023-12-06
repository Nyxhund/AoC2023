module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Char

readNums :: [String] -> [Int]
readNums = map read

predicate :: Int -> Int -> Int -> Int
predicate x y lim =
            if x * y > lim then
                y
            else
                predicate (x-1) (y+1) lim

getRange :: (Int, Int) -> Int
getRange (t, d) = length $ filter (>d) [i * (t - i) | i <- [0..t]]

main = do
    file <- readFile "input.txt"
    let [times, distances] = map (readNums . words . drop 9) $ lines file
    let timesAndDistances = zip times distances

    putStrLn "Exo 1:"
    print $ product $ map getRange timesAndDistances

    putStrLn "Exo 2:"
    let [time, dist] = readNums $ map (concat . words . drop 9) $ lines file

    print $ getRange (time, dist)
