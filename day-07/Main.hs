module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Char

parseInput :: String -> [(String, Int)]
parseInput = map ((\[c, nb] -> (c, read nb)) . words) . lines

splitString :: String -> [Int]
splitString [] = []
splitString str = length subStr : splitString rest
        where (subStr, rest) = span (== head str) str

charToIntVal :: Char -> Int
charToIntVal 'A' = 14
charToIntVal 'K' = 13
charToIntVal 'Q' = 12
charToIntVal 'J' = 11
charToIntVal 'T' = 10
charToIntVal c = ord c - ord '0'

charToIntValPart2 :: Char -> Int
charToIntValPart2 'J' = 0
charToIntValPart2 c = charToIntVal c

handValue :: String -> Int
handValue hand | 5 `elem` groups                        = 6  -- 5 of a kind
               | 4 `elem` groups                        = 5  -- 4 of a kind
               | 3 `elem` groups && 2 `elem` groups     = 4  -- full house
               | 3 `elem` groups                        = 3  -- 3 of a kind
               | length (filter (==2) groups) == 2      = 2  -- 2 pairs
               | 2 `elem` groups                        = 1  -- 1 pairs
               | otherwise                              = 0  -- Highcard
        where groups = (splitString . sort) hand

handValuePart2 :: String -> Int
handValuePart2 hand | 5 - jokerNb `elem` groups           = 6  -- 5 of a kind
                    | jokerNb == 5                        = 6  -- 5 of a kind
                    | 4 - jokerNb `elem` groups           = 5  -- 4 of a kind
                    | 3 `elem` groups && 2 `elem` groups  = 4  -- full house
                    | jokerNb == 1 && normalVal == 2      = 4
                    | 3 - jokerNb `elem` groups           = 3  -- 3 of a kind
                    | length (filter (==2) groups) == 2   = 2  -- 2 pairs
                    | 2 - jokerNb `elem` groups           = 1  -- 1 pairs
                    | otherwise                           = 0  -- Highcard
        where groups = (splitString . sort) normalHand
              normalHand = filter (/='J') hand
              jokerNb = length $ filter (=='J') hand
              normalVal = handValue normalHand

sorting :: (String -> Int) -> (Char -> Int) -> [(String, Int)] -> [(String, Int)]
sorting handVal charToInt = sortBy (\s1 s2 ->
                let (s1Val, s2Val) = ((handVal . fst) s1, (handVal . fst) s2) in
                if s1Val == s2Val then
                    compare (map charToInt (fst s1)) (map charToInt (fst s2))
                else
                    compare s1Val s2Val)

sumScore :: [(String, Int)] -> Int
sumScore = sum . zipWith (curry (\(rank, (_, val)) -> rank * val)) [1..]

main = do
    file <- readFile "input.txt"
    let cards = parseInput file
    let sorted = sorting handValue charToIntVal cards

    putStrLn "Exo 1:"
    print $ sumScore sorted

    putStrLn "Exo 2:"
    let sortedPart2 = sorting handValuePart2 charToIntValPart2 cards
    print $ sumScore sortedPart2
