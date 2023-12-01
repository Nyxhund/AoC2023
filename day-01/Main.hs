module Main where

import Data.Char
import Data.List
import System.IO

stringToNum :: String -> Char
stringToNum str | "one" `isPrefixOf` str = '1'
                | "two" `isPrefixOf` str = '2'
                | "three" `isPrefixOf` str = '3'
                | "four" `isPrefixOf` str = '4'
                | "five" `isPrefixOf` str = '5'
                | "six" `isPrefixOf` str = '6'
                | "seven" `isPrefixOf` str = '7'
                | "eight" `isPrefixOf` str = '8'
                | "nine" `isPrefixOf` str = '9'
                | otherwise = head str

convertToString :: String -> String
convertToString "" = ""
convertToString str = stringToNum str : convertToString (drop 1 str)

main = do
  putStrLn "Exo 1:"
  file <- readFile "input.txt"
  print $ show $ sum $ map read $ map (\s -> [head s, last s]) $ map (filter isDigit) $ lines file
  putStrLn "Exo 2:"
  print $ show $ sum $ map read $ map (\s -> [head s, last s]) $ map (filter isDigit) $ map convertToString $ lines file
