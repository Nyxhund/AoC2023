module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Char

convertSeeds :: [String] -> [Int]
convertSeeds str = map read $ words $ drop 6 $ concat str

toMap :: [[[String]]] -> [[[Int]]]
toMap = map (map (map read))

toBlocks :: [String] -> [String] -> [[String]]
toBlocks [] accu = [accu]
toBlocks (curr : tail) accu =
            if curr == "" then
                accu : toBlocks (drop 1 tail) []
            else
                toBlocks tail (curr : accu)

checkMap :: Int -> [[Int]] -> Int
checkMap nb [] = nb
checkMap nb (range : tail) =
            if src <= nb && nb < src + rng then
                nb + (dest - src)
            else
                checkMap nb tail
            where [dest, src, rng] = range

fuseRange :: (Int, Int, Int) -> (Int, Int, Int) -> ([(Int, Int, Int)], [(Int, Int, Int)])
fuseRange a@(aLow, aHigh, aFun) b@(bLow, bHigh, bFun)
            | bHigh < aInBLow || aInBHigh < bLow = ([a], [b]) -- A and B don't overlap

            | aInBLow < bLow && bHigh < aInBHigh = -- B contained in A
              ([(aLow, bLow - aFun, aFun),
                (bLow - aFun, bHigh - aFun, aFun + bFun),
                (bHigh - aFun, aHigh, aFun)], [])

            | bLow < aInBLow && aInBHigh < bHigh = -- A contained in B
              ([(aLow, aHigh, aFun + bFun)],
               [(bLow, aInBLow, bFun), (aInBHigh, bHigh, bFun)])

            | aInBHigh < bHigh =
              ([(aLow, bLow - aFun, aFun), (bLow - aFun, aHigh, aFun + bFun)],
               [(aInBHigh, bHigh, bFun)])

            | aInBHigh > bHigh =
              ([(aLow, bHigh - aFun, aFun + bFun), (bHigh - aFun, aHigh, aFun)],
               [(bLow, aInBLow, bFun)])

            | otherwise = ([(aLow, aHigh, aFun + bFun)], []) -- A and B overlap
        where aInBLow = aLow + aFun
              aInBHigh = aHigh + aFun

fuseMaps :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
fuseMaps [] b = b
fuseMaps (curr : tail) b = aVals ++ fuseMaps tail bVals
        where fused = map (fuseRange curr) b
              aVals = concatMap fst fused
              bVals = concatMap snd fused

main = do
    file <- readFile "example.txt"

    let (seedsList, maps) = span (/=[]) $ lines file

    let seeds = convertSeeds seedsList
    let blocks = toBlocks (drop 2 maps) []
    -- print "----------------------------"
    let maps = toMap $ map (map words) blocks
    -- mapM print maps
    -- print "----------------------------"
    let ranges = map (map (\[dst, src, range] -> (src, src + range - 1, dst - src))) maps
    mapM print ranges

    putStrLn "Exo 1:"
    print $ minimum $ map (\nb -> foldl checkMap nb maps) seeds

    -- [(0,68,1),(69,69,-69)]
    -- [(93,96,-37),(56,92,4)]

    putStrLn "Exo 2:"
    print "Input is:"
    print "(69, 69, -69) (56, 92, 4)"
    let t = fuseMaps [(0,68,1),(69,69,-69)] [(56,92,4)]

    mapM print t
