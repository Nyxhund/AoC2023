module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Char
import Data.Graph

type Coordinates = (Int, Int)
type Position = (Coordinates, Char)
type Mapped = [Position]
type Node = (Char, Coordinates, [Coordinates])

adjList :: Position -> Node
adjList ((y, x), c) = (c, (y, x), adj)
        where adj | c == '|' = [(y+1, x), (y-1, x)]
                  | c == '-' = [(y, x+1), (y, x-1)]
                  | c == 'L' = [(y-1, x), (y, x+1)]
                  | c == 'J' = [(y-1, x), (y, x-1)]
                  | c == '7' = [(y+1, x), (y, x-1)]
                  | c == 'F' = [(y+1, x), (y, x+1)]
                  | c == '.' = []
                  | c == 'S' = []

updateS :: [Coordinates] -> Node -> Node
updateS adj ('S', coords, _) = ('S', coords, adj)
updateS _ n = n

-- I go through pairing up 2 "Walls" together, to get the range between them
-- Turns out pairing walls that can be 2 characters wide is not very straight forward
-- Also, I did not replace 'S' with it's proper equivalent character but I really can't anymore ;-;
-- (It should be one more matching to change `updateS` to the appropriate Char)
getRange :: [Node] -> [(Coordinates, Coordinates)]
getRange [] = []
getRange [_] = []
getRange ((c1, coord1@(y1, x1), _) : second@(c2, coord2@(y2, x2), _) : tail)
            | y1 /= y2               = getRange (second : tail)
            | c1 == '-'              = getRange (dropWhile (\(c, _, _) -> c == '-') tail)
            | c2 == '-'              = getRange next
            | c1 == 'F' && c2 == 'J' = getRange (second : tail)
            | c1 == 'L' && c2 == '7' = getRange (second : tail)
            | otherwise    = (coord1, coord2) : getRange next
                where withoutDashes = dropWhile (\(c, _, _) -> c == '-') tail
                      (nextChar, _, _) | null withoutDashes = ('0', (0, 0), [])
                                       | otherwise = head withoutDashes
                      next | c1 == 'F' && nextChar == '7' = drop 1 withoutDashes
                           | c1 == 'L' && nextChar == 'J' = drop 1 withoutDashes
                           | c2 == 'L' && nextChar == '7' = drop 1 withoutDashes
                           | c2 == 'F' && nextChar == 'J' = drop 1 withoutDashes
                           | otherwise                    = withoutDashes

main = do
    file <- readFile "input.txt"
    let mapped = map (\(y, l) -> map (\(x, c) -> ((y, x), c)) l) $ zip [0..] $ map (zip [0..]) $ lines file
    let nodes = map adjList (concat mapped)
    let (_, sCoords, _) = head $ dropWhile (\(s, _, _) -> s /= 'S') nodes
    let sAdjacent = map (\(_, coords, _) -> coords) $ filter (\(_, _, adj) -> sCoords `elem` adj) nodes

    let updatedNodes = map (updateS sAdjacent) nodes
    let (graph, nfv, vfk) = graphFromEdges updatedNodes
    putStrLn "Exo 1:"
    case vfk sCoords of
        Just v -> print $ flip div 2 $ length $ reachable graph v
        Nothing -> error "Source point not found"

    putStrLn "Exo 2:"
    let pipe = case vfk sCoords of
                Just v -> map nfv $ reachable graph v
                Nothing -> error "Source point not found"
    let sorted = sortBy (\(_, c1, _) (_, c2, _) -> compare c1 c2) pipe
    let walls = getRange sorted
    let range = map (\((_, x1), (_, x2)) -> x2 - x1 - 1) walls
    -- mapM print $ filter (\t -> snd t /= 0) $ zip walls range
    print $ sum range
