module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Char

readNumbers :: [[[String]]] -> [[[Int]]]
readNumbers = map (map (map read))

cascadeScratchcards :: [Int] -> [Int] -> [Int]
cascadeScratchcards _ [] = []
cascadeScratchcards (nb : tail) (matches : tmatches) = nb : cascadeScratchcards updated tmatches
            where updated = zipWith (+) tail (replicate matches nb) ++ drop matches tail

main = do
    file <- readFile "input.txt"
    let cards = map (map (filter (/= "") . splitOn " ") . splitOn "|" . drop 1 . dropWhile (/= ':')) $ lines file
    let numberCards = readNumbers cards
    let matches = map (length . \[winning, drawn] -> winning `intersect` drawn) numberCards

    putStrLn "Exo 1:"
    print $ sum $ map (\i -> 2 ^ (i-1)) $ filter (/=0) matches

    putStrLn "Exo 2:"
    print $ sum $ cascadeScratchcards (replicate (length matches) 1) matches
