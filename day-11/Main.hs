module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Char

type Coordinates = (Int, Int)
type Position = (Coordinates, Char)

addCoordinates :: [String] -> [[Position]]
addCoordinates = map (\(y, l) -> map (\(x, c) -> ((y, x), c)) l) . zip [0..] . map (zip [0..])

getDistance :: Coordinates -> Coordinates -> Int
getDistance (y1, x1) (y2, x2) = abs (y2 - y1) + abs (x2 - x1)

expandGalaxy :: [String] -> [String]
expandGalaxy [] = []
expandGalaxy (s : tail)
            | all (=='.') s = s : s : expandGalaxy tail
            | otherwise     = s : expandGalaxy tail

getGalaxyExpansion :: [[Position]] -> (Coordinates -> Int) -> [Int]
getGalaxyExpansion [] _ = []
getGalaxyExpansion (line : tail) fn
                | all ((=='.') . snd) line = (fn . fst . head) line : getGalaxyExpansion tail fn
                | otherwise                = getGalaxyExpansion tail fn

getDistanceWithExpansion :: Coordinates -> Int -> [Int] -> [Int] -> Coordinates -> Int
getDistanceWithExpansion (y1, x1) extension ys xs (y2, x2) = abs (y2 - y1) + abs (x2 - x1) + xJumps + yJumps - xLines - yLines
                where xJumps = extension * xLines
                      xLines = length (filter (\x -> x1 < x && x < x2
                                                  || x2 < x && x < x1) xs)
                      yJumps = extension * yLines
                      yLines = length (filter (\y -> y1 < y && y < y2
                                                  || y2 < y && y < y1) ys)

main = do
    file <- readFile "input.txt"
    let yExpanded = expandGalaxy $ lines file
    let expanded = expandGalaxy $ transpose yExpanded
    let expandedMapped = addCoordinates expanded
    let galaxies = filter (\(_, c) -> c == '#') $ concat expandedMapped
    let distances = concatMap (\g -> map (getDistance (fst g) . fst) galaxies) galaxies
    putStrLn "Exo 1:"
    print $ flip div 2 $ sum distances

    putStrLn "Exo 2:"
    let mapped = addCoordinates $ lines file
    let yLines = getGalaxyExpansion mapped fst
    let transposedMap = transpose mapped
    let xLines = getGalaxyExpansion transposedMap snd
    let galaxiesBaseMap = filter (\(_, c) -> c == '#') $ concat transposedMap
    let distancesPart2 = concatMap (\g -> map (getDistanceWithExpansion (fst g) 1000000 yLines xLines . fst) galaxiesBaseMap) galaxiesBaseMap
    print $ flip div 2 $ sum distancesPart2

    putStrLn "Exo 1 reworked:"
    let distancesPart1 = concatMap (\g -> map (getDistanceWithExpansion (fst g) 2 yLines xLines . fst) galaxiesBaseMap) galaxiesBaseMap
    print $ flip div 2 $ sum distancesPart1
