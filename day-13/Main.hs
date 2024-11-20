module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Char

blocks :: [String] -> [String] -> [[String]]
blocks accu [] = [accu]
blocks accu (s : tail)
        | s == ""   = accu : blocks [] tail
        | otherwise = blocks (accu ++ [s]) tail

hasMirror :: Int -> String -> String -> [Int]
hasMirror _ _ [] = []
hasMirror i reversed curr@(c : tail)
        | curr `isPrefixOf` reversed
          || reversed `isPrefixOf` curr = i : hasMirror (i + 1) (c : reversed) tail
        | otherwise = hasMirror (i + 1) (c : reversed) tail

retrieveCommonLine :: [[Int]] -> Int
retrieveCommonLine (l : tail) = ret
            where ret | null inter = -1
                      | otherwise  = head inter
                  inter = foldl intersect l tail

getReflection :: [String] -> Int
getReflection = retrieveCommonLine . map (\s -> hasMirror 1 [head s] (drop 1 s))

getVal :: Int -> Int -> Int
getVal (-1) nb = nb * 100
getVal nb _ = nb

getDifferences :: String -> String -> Int -> [Int]
getDifferences [] [] _ = []
getDifferences _ []  _ = []
getDifferences [] _  _ = []
getDifferences (s1 : tail1) (s2 : tail2) i
        | s1 == s2  = getDifferences tail1 tail2 (i+1)
        | otherwise = i : getDifferences tail1 tail2 (i+1)

possibleLines :: Int -> String -> String -> [Int]
possibleLines _ _ [] = []
possibleLines i reversed curr@(c : tail)
        | length pos == 1    = i - 1 - head pos : i + head pos : possibleLines (i + 1) (c : reversed) tail
        | otherwise          = possibleLines (i + 1) (c : reversed) tail
            where pos = getDifferences reversed curr 0

main = do
    file <- readFile "example.txt"

    let b = map (\b -> (b, transpose b)) $ blocks [] $ lines file
    let mirrors = map (\(x, y) -> getVal (getReflection x) (getReflection y)) b
    putStrLn "Exo 1:"
    print $ sum mirrors

    putStrLn "Exo 2:"
    mapM print $ fst $ head b
    let votesX = map (\s -> possibleLines 1 [head s] (drop 1 s)) $ fst $ head b
    let votesXMapped = concatMap (\(y, xL) -> map (\x -> (y, x)) xL) $ zip [0..] votesX
    print "--------------------"
    let votesY = map (\s -> possibleLines 1 [head s] (drop 1 s)) $ snd $ head b
    let votesYMapped = concatMap (\(x, yL) -> map (\y -> (y, x)) yL) $ zip [0..] votesY
    let votes = group $ sort $ votesXMapped ++ votesYMapped
    mapM print votes
