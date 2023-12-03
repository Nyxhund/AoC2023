module Main where

import System.IO
import Data.List.Split
import Data.Char

getXCoordinates :: (Char -> Bool) -> [(Int, Char)] -> [Int]
getXCoordinates _ [] = []
getXCoordinates p ((x, c) : l) =
        if p c then  -- Could do wth filter
            x : getXCoordinates p l
        else
            getXCoordinates p l

getYCoordinates = concatMap (\(y, l) -> zip (repeat y) (getXCoordinates (\c -> not $ isDigit c || c == '.') l))

secondGetYCoordinates = concatMap (\(y, l) -> zip (repeat y) (getXCoordinates (== '*') l))

getNumbers :: [(Int, Char)] -> [(Int, Int, Int)]
getNumbers [] = []
getNumbers l = number : getNumbers rest
        where (nb, rest) = span (\(i, c) -> isDigit c) startNb
              startNb = dropWhile (\(i, c) -> not $ isDigit c) l
              number =
                if nb /= [] then
                    (fst $ head nb, fst $ last nb, read $ map snd nb)
                else
                    (0, 0, 0)

generateInfluence :: (Int, Int) -> [(Int, Int)]
generateInfluence (y, x) = [(y-1, x-1), (y-1, x),(y-1, x+1),(y, x-1),(y, x+1),(y+1, x-1),(y+1, x),(y+1, x+1)]

nbPresence :: Int -> Int -> Int -> [(Int, Int)]
nbPresence y s f = take (f - s + 1) $ zip (repeat y) [s..]

affectedSquares :: [(Int, Int)] -> [(Int, Int)]
affectedSquares = concatMap generateInfluence

filterList :: [(Int, Int)] -> (Int, [(Int, Int, Int)]) -> (Int, [(Int, Int, Int)])
filterList influence (y, l) = (y, filter (\(s, f, nb) -> any (`elem` influence) (nbPresence y s f)) l)

main = do
    file <- readFile "input.txt"
    putStrLn "Exo 1:"
    let coordinatesMap = zip [0..] $ map (zip [0..]) $ lines file
    let coordinates = getYCoordinates coordinatesMap
    let numbersStartFinish = zip [0..] $ map (filter (/= (0, 0, 0)) . getNumbers . snd) coordinatesMap

    let influence = affectedSquares coordinates
    let collectedNb = concatMap (map (\(s, f, nb) -> nb) . snd) $ map (filterList influence) numbersStartFinish
    print $ sum collectedNb
    putStrLn "Exo 2:"

    let coordinatesPart2 = secondGetYCoordinates coordinatesMap
    let influencePart2 = affectedSquares coordinatesPart2
    let collectedNb2 = map (filterList influencePart2) numbersStartFinish

    mapM print coordinatesPart2
    mapM print numbersStartFinish
    print ""
    let numsPerGear = map (\coord -> map (filterList (generateInfluence coord)) collectedNb2) coordinatesPart2
    print $ sum $ map product $ filter (\l -> length l == 2) $ map (map (\(_, _, nb) -> nb) . map (\(_, l) -> head l) . filter (\(_, l) -> l /= [])) numsPerGear

    --mapM print collectedNb2
    --let numbersPerGear = sum $ map product $ filter (\l -> length l == 2) $ map (map (\(_, _, nb) -> nb) . map (\(_, l) -> head l) . filter (\(_, l) -> l /= []) . \coord -> map (filterList (generateInfluence coord)) collectedNb2) coordinatesPart2
    -- print numbersPerGear -- 65913795 too low
