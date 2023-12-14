module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Char

slideNorthWest :: String -> String
slideNorthWest [] = []
slideNorthWest s =  (sortBy (flip compare) toSort) ++ squares ++ slideNorthWest tail
        where (toSort, rest) = span (/= '#') s
              (squares, tail) = span (== '#') rest

slideSouthEast :: String -> String
slideSouthEast [] = []
slideSouthEast s =  (sort toSort) ++ squares ++ slideSouthEast tail
        where (toSort, rest) = span (/= '#') s
              (squares, tail) = span (== '#') rest

score :: String -> Int
score s = sum $ map fst $ filter (\t -> snd t == 'O') $ zip [l, l - 1 .. 1] s
        where l = length s

cycleBoard :: [String] -> [String]
cycleBoard = transpose . map slideSouthEast . transpose . map slideSouthEast . transpose . map slideNorthWest . transpose . map slideNorthWest

main = do
    file <- readFile "example.txt"
    let grid = transpose $ lines file
    --mapM print grid

    putStrLn "Exo 1:"
    let slidedNorth = map slideNorthWest grid
    --mapM print slidedNorth
    print $ sum $ map score slidedNorth

    putStrLn "Exo 2:"
    print $ sum $ map score $ head $ drop 999999999 $ iterate cycleBoard grid
