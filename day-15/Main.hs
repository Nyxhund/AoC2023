module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Char
import Data.Map (adjust, fromList, (!))
import qualified Data.Map as M

hash :: String -> Int
hash = foldl (\accu -> flip mod 256 . (*) 17 . (+) accu . ord) 0

updateList :: [(String, Int)] -> (String, Int) -> [(String, Int)]
updateList l t@(label, focus)
               | label `elem` map fst l = map (\(lab, foc) -> if lab == label then (lab, focus) else (lab, foc)) l
               | otherwise  = l ++ [t]

updateMap :: M.Map Int [(String, Int)] -> String -> M.Map Int [(String, Int)]
updateMap m s | op == "-" =    adjust (filter (\(lab, _) -> lab /= label)) box m
              | op == "=" =    adjust (\l -> updateList l (label, num)) box m
        where label = takeWhile (\c -> c /= '-' && c /= '=') s
              box   = hash label
              op    = take 1 $ dropWhile (\c -> c /= '-' && c /='=') s
              num   = read $ drop 1 $ dropWhile (\c -> c /= '-' && c /='=') s

arrangeBoxes :: [String] -> M.Map Int [(String, Int)]
arrangeBoxes = foldl updateMap (fromList (zip [0..255] (repeat [])))

score :: M.Map Int [(String, Int)] -> Int -> Int
score m i = sum $ map (\(index, foc) -> index * foc * (i+1)) $ zip [1..] l
        where l = map snd $ m!i

main = do
    file <- readFile "input.txt"

    let input = splitOn "," $ head $ lines file
    putStrLn "Exo 1:"
    print $ sum $ map hash input

    putStrLn "Exo 2:"
    let arranged = arrangeBoxes input
    print $ sum $ map (score arranged) [0..255]
