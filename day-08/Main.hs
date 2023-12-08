module Main where

import System.IO
import Data.List.Split
import Data.List
import Data.Char
import Data.Graph

getBranching :: Char -> (String, String, [String]) -> String
getBranching c (_, _, [left, right]) = case c of
                                         'L' -> left
                                         'R' -> right

nextEdge :: Char -> (Vertex -> (String, String, [String])) -> Maybe Int -> String
nextEdge c nodeFromVertex curr = case curr of
                                 Just i  -> getBranching c $ nodeFromVertex i
                                 Nothing -> error "Key not in graph"

traverseGraph ::(Vertex -> (String, String, [String])) -> (String -> Maybe Vertex) -> String -> String -> Int
traverseGraph _ _ _ s | last s == 'Z' = 0
traverseGraph nodeFromVertex vertexFromKey (c : tail) current =
            1 + traverseGraph nodeFromVertex vertexFromKey (tail ++ [c]) (nextEdge c nodeFromVertex vertex)
                    where vertex = vertexFromKey current

main = do
    file <- readFile "input.txt"
    let (instructions, links) = (\(inst, links) -> (head inst, drop 1 links)) $ span (/=[]) $ lines file
    let edgeList = map ((\[node, list] -> (node, splitOn ", " $ drop 1 $ dropWhileEnd (==')') list)) . splitOn " = ") links

    let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges $ map (\(node, l) -> (node, node, l)) edgeList

    putStrLn "Exo 1:"
    print $ traverseGraph nodeFromVertex vertexFromKey instructions "AAA"

    putStrLn "Exo 2:"
    let aNodes = filter (\s -> last s == 'A') $ map fst edgeList
    print $ foldl lcm 1 $ map (traverseGraph nodeFromVertex vertexFromKey instructions) aNodes

