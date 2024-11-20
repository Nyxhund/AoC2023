module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Char

fact :: Int -> Int
fact 0 = 1
fact n = n * fact n - 1

explore :: [(Int, Char)] -> [Int] -> Int
explore [] [] = 1
explore [(_, '.')] [] = 1
explore [(_, '#')] [] = 0
explore [(_, '?')] [] = 0
explore [] [_] = 0
explore [(nb, info)] [count]
        | info == '?' && count == nb = 1
        | info == '?' && count  < nb = res
        | info == '#' && count == nb = 1
        | otherwise                  = 0
            where nFact = fact nb
                  kFact = fact count
                  bothFact = fact (nb - count)
                  res = nFact `div` kFact * bothFact

explore spring@((nb, info) : (after, afterInfo) : tailSpring) countList@(count : tailCount)
        | info == '#' && afterInfo == '.' && count == nb   = explore tailSpring tailCount
        | info == '#' && afterInfo == '.' && count > nb    = 0
        | info == '#' && afterInfo == '.' && count < nb    = 0

        | info == '#' && afterInfo == '?' && count == nb   = explore ((after-1, afterInfo) : tailSpring) tailCount
        | info == '#' && afterInfo == '?' && count < nb    = 0
        | info == '#' && afterInfo == '?' && count > reach = 0
        | info == '#' && afterInfo == '?' && count > nb + after = explore tailSpring (count - nb - after : tailCount)
        | info == '#' && afterInfo == '?' && count < nb + after = explore ((after - (count - nb) - 1, afterInfo) : tailSpring) tailCount
        | info == '#' && afterInfo == '?' && count == nb + after = explore tailSpring tailCount

        | info == '?' && nb <= 0   = explore ((after, afterInfo) : tailSpring) countList
        | info == '?' && afterInfo == '.' && (count == nb || count == nb + 1) = explore tailSpring tailCount
        | info == '?' && afterInfo == '.' && count > nb   = explore tailSpring countList
        | info == '?' && afterInfo == '.' && count < nb + 1  = explore ((nb - count, info) : tailSpring) tailCount
            where reach = sum $ map fst $ takeWhile (\s -> '.' /= snd s) spring

main = do
    file <- readFile "example.txt"

    putStrLn "Exo 1:"
    let stringsSeparated = map ((\(spring, nbs) -> (map (\s -> (length s, head s)) $ group spring, map read $ splitOn "," $ drop 1 nbs)) . span (/=' ')) $ lines file :: [([(Int, Char)], [Int])]
    mapM print stringsSeparated

    putStrLn "Exo 2:"
