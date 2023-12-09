module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Char

diffs :: [Int] -> [Int]
diffs [_] = []
diffs (a : t) = a - head t : diffs t

getTower :: [[Int]] -> [[Int]]
getTower curr@(h : _) | all (==0) h = curr
getTower curr@(h : _) = getTower (diffs h : curr)

predict :: [[Int]] -> Int
predict = foldl (\acc l -> head l + acc) 0

predictPast :: [[Int]] -> Int
predictPast = foldl (\acc l -> last l - acc) 0

main = do
    file <- readFile "input.txt"

    let l = map (reverse . map read . words) $ lines file :: [[Int]]
    putStrLn "Exo 1:"
    print $ sum $ map (predict . \t -> getTower [t]) l
    putStrLn "Exo 2:"
    print $ sum $ map (predictPast . \t -> getTower [t]) l
