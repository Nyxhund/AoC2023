module Main where

import System.IO
import Data.List.Split
import Data.Char

getXCoordinates :: (Char -> Bool) -> [((Int, Int), Char)] -> [(Int, Int)]
getXCoordinates p l = map fst $  filter (\(_, c) -> p c) l

getYCoordinates :: (Char -> Bool) -> [[((Int, Int), Char)]] -> [(Int, Int)]
getYCoordinates = concatMap . getXCoordinates

getNumberInLine :: [((Int, Int), Char)] -> [([(Int, Int)], Int)]
getNumberInLine [] = []
getNumberInLine l = if nb /= [] then
                        (map fst nb, read $ map snd nb) : getNumberInLine rest
                    else
                        getNumberInLine rest
                    where startNb = dropWhile (\(_, c) -> not $ isDigit c) l
                          (nb, rest) = span (\(_, c) -> isDigit c) startNb

generateInfluence :: (Int, Int) -> [(Int, Int)]
generateInfluence (y, x) = [(y-1, x-1), (y-1, x),(y-1, x+1),(y, x-1),(y, x+1),(y+1, x-1),(y+1, x),(y+1, x+1)]

affectedSquares :: [(Int, Int)] -> [(Int, Int)]
affectedSquares = concatMap generateInfluence

numbersInInfluence influence numbers = map snd $ filter (\(coord, _) -> any (`elem` influence) coord) numbers

main = do
    file <- readFile "input.txt"
    putStrLn "Exo 1:"
    let coordinates = map (\(y, l) -> map (\(x, c) -> ((y, x), c)) l) . zip [0..] . map (zip [0..]) $ lines file

    let coordinatesAllSymbols = getYCoordinates (\c -> not $ isDigit c || c == '.') coordinates
    -- print "Coordinates of all symbols"
    -- print coordinatesAllSymbols

    let numbersInLine = concatMap getNumberInLine coordinates
    -- print "Coordinates of all numbers"
    -- mapM print numbersInLine

    let influence = affectedSquares coordinatesAllSymbols
    -- print influence
    let pointsInInfluence = numbersInInfluence influence numbersInLine
    print $ sum pointsInInfluence

    putStrLn "Exo 2:"

    let coordinatesStars = getYCoordinates (== '*') coordinates
    let pointsPerStar = filter (\l -> length l == 2) $ map (\coords -> numbersInInfluence (affectedSquares [coords]) numbersInLine) coordinatesStars
    -- mapM print pointsPerStar
    print $ sum $ map product pointsPerStar
