{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import qualified Data.List as L
import Data.Function (on)
import System.IO
import System.Environment
import Control.Monad
import Data.Graph
import Data.Maybe (fromJust)

type Point = ([Double], Int)
type Connection = (Int, Int, Double)

-- Leitura CSV
readInputFile :: FilePath -> IO [Point]
readInputFile file = do
    content <- readFile file
    let ls = lines content
    return $ zip (map (map read . splitComma) ls) [1..]
  where
    splitComma = map removeSpaces . splitBy ','
    splitBy c = foldr f [[]]
      where
        f x acc@(y:ys)
          | x == c    = [] : acc
          | otherwise = (x : y) : ys
    removeSpaces = filter (/= ' ')

-- Distância euclidiana
euclideanDistance :: [Double] -> [Double] -> Double
euclideanDistance a b = sqrt $ sum $ zipWith (\x y -> (x - y) ** 2) a b

-- Retorna o índice e a distância do ponto mais próximo
getNearestPoint :: [Point] -> Point -> (Int, Double)
getNearestPoint ps a = L.minimumBy (compare `on` snd) $
    zip [0..] (map (euclideanDistance (fst a) . fst) ps)

-- Conecta os pontos sequencialmente
getConnections :: [Point] -> [Connection]
getConnections [] = []
getConnections (start:rest) = go start rest []
  where
    go _ [] acc = acc
    go curr pl acc =
      let (idx, dist) = getNearestPoint pl curr
          next = pl !! idx
          newList = take idx pl ++ drop (idx + 1) pl
          conn = (snd curr, snd next, dist)
      in go next newList (conn : acc)

-- Remove maiores arestas e retorna grupos
findGroups :: Int -> Int -> [Connection] -> [[Int]]
findGroups k n conns =
    let sorted = reverse $ L.sortBy (compare `on` (\(_,_,d) -> d)) conns
        trimmed = drop (k - 1) sorted
        edges = map (\(a,b,_) -> (a, b)) trimmed
        allNodes = [1..n]
        adjList = [(i, i, neighbors i edges) | i <- allNodes]
        (graph, _, vertexFromKey) = graphFromEdges adjList
        comps = components graph
    in map (L.sort . map (\v -> let (k,_,_) = vertexFromKey v in k)) comps
  where
    neighbors i es = [x | (a, x) <- es, a == i] ++ [x | (x, b) <- es, b == i]

-- Escreve arquivo CSV
writeOutputFile :: FilePath -> [[Int]] -> IO ()
writeOutputFile file groups = do
    let lines = map (L.intercalate "," . map show) groups
    writeFile file (unlines lines)

-- Função principal
main :: IO ()
main = do
    putStrLn "Forneça o nome do arquivo de entrada: "
    input <- getLine
    putStrLn "Forneça o nome do arquivo de saída: "
    output <- getLine
    putStrLn "Forneça o número de grupos (K): "
    kStr <- getLine
    let k = read kStr :: Int

    points <- readInputFile input
    let conns = getConnections points
    let groups = findGroups k (length points) conns
    writeOutputFile output groups

    putStrLn "Agrupamentos:"
    mapM_ (putStrLn . L.intercalate ", " . map show) groups
