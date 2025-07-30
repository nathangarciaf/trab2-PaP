import System.IO
import System.Directory (doesFileExist)
import Data.List (minimumBy, sortBy, delete, nub, sort, intercalate)
import Data.Ord (comparing)
import qualified Data.Map as Map

main :: IO ()
main = do
    putStrLn "Forneça o nome do arquivo de entrada: "
    inputFileName <- getLine

    putStrLn "Forneça o nome do arquivo de saída: "
    outputFileName <- getLine

    putStrLn "Forneça o número de grupos (K): "
    kStr <- getLine
    let k = read kStr :: Int

    exists <- doesFileExist inputFileName
    if exists
        then do
            points <- readInputFile inputFileName
            let connections = getConnections points
                sortedConnections = sortBy (comparing (\(_, _, d) -> d)) connections
                trimmedConnections = take (length sortedConnections - (k - 1)) sortedConnections
                groups = buildGroups (length points) trimmedConnections

            putStrLn "\nAgrupamentos:"
            mapM_ printGroup (zip [1..] groups)

            writeFile outputFileName (unlines $ map formatGroup (zip [1..] groups))
        else
            putStrLn $ "Erro: o arquivo '" ++ inputFileName ++ "' não foi encontrado ou não pode ser lido."

-- Imprime um grupo no terminal
printGroup :: (Int, [Int]) -> IO ()
printGroup (_gid, members) =
    putStrLn $ intercalate "," $ map show $ sort members

-- Formata um grupo para escrever no arquivo
formatGroup :: (Int, [Int]) -> String
formatGroup (_gid, members) =
    intercalate "," $ map show $ sort members

-- Algoritmo Union-Find para formar grupos
buildGroups :: Int -> [(Int, Int, Float)] -> [[Int]]
buildGroups n edges =
    let parents = [0..n]  -- índice 0 ignorado, usamos índices começando de 1
        finalParents = foldl (\ps (a, b, _) -> union a b ps) parents edges
        groupsMap = foldl
            (\m i ->
                let root = find i finalParents
                in Map.insertWith (++) root [i] m
            )
            Map.empty
            [1..n]
    in Map.elems groupsMap

-- Encontra o representante do grupo
find :: Int -> [Int] -> Int
find x parents
    | parents !! x == x = x
    | otherwise = find (parents !! x) parents

-- Une dois conjuntos
union :: Int -> Int -> [Int] -> [Int]
union a b parents =
    let rootA = find a parents
        rootB = find b parents
    in if rootA == rootB then parents else replaceAt rootB rootA parents

-- Substitui o valor em um índice
replaceAt :: Int -> Int -> [Int] -> [Int]
replaceAt idx val xs =
    take idx xs ++ [val] ++ drop (idx + 1) xs

-- Leitura do arquivo CSV
readInputFile :: FilePath -> IO [([Float], Int)]
readInputFile filename = do
    contents <- readFile filename
    let linesOfFile = lines contents
    return $ zipWith parseLine [1..] linesOfFile

-- Divide uma string por vírgula
splitByComma :: String -> [String]
splitByComma [] = [""]
splitByComma (c:cs)
    | c == ','  = "":rest
    | otherwise = (c : head rest) : tail rest
  where
    rest = splitByComma cs

-- Parser da linha CSV
parseLine :: Int -> String -> ([Float], Int)
parseLine idx line =
    let values = splitByComma line
        floats = map (read . trim) values
    in (floats, idx)

-- Remove espaços
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (`elem` " \t\r\n")

-- Distância euclidiana
calculateDistance :: [Float] -> [Float] -> Float
calculateDistance a b = sqrt . sum $ zipWith (\x y -> (x - y) ^ 2) a b

-- Encontra o ponto mais próximo
getNearestPoint :: [([Float], Int)] -> [Float] -> (Int, Float)
getNearestPoint points current =
    let distances = zipWith (\(coords, _) i -> (i, calculateDistance current coords)) points [0..]
    in minimumBy (comparing snd) distances

-- Conexões em MST com abordagem greedy
getConnections :: [([Float], Int)] -> [(Int, Int, Float)]
getConnections [] = []
getConnections (start:rest) = go start rest []
  where
    go _ [] path = reverse path
    go current remaining path =
        let (idx, dist) = getNearestPoint remaining (fst current)
            nearest = remaining !! idx
            newRemaining = take idx remaining ++ drop (idx + 1) remaining
            newPath = (snd current, snd nearest, dist) : path
        in go nearest newRemaining newPath

-- Formata conexão para saída
formatConnection :: (Int, Int, Float) -> String
formatConnection (a, b, d) = show a ++ " -> " ++ show b ++ " (distância: " ++ show d ++ ")"
