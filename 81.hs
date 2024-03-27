import Graph
import Data.Array (Ix, Array, range, bounds, (!), array, listArray)
import Data.List.Split (splitOn)

main = do
    file <- readFile "/home/ssmike/Downloads/0081_matrix.txt"
    let nums = map (map (read :: String -> Int) . splitOn ",") (lines file)
    let n = length nums
    let field = listArray (1, n) [listArray (1, n) row | row <- nums]
    let neighbours i j = [((i+1,j), field!(i+1)!j) | i < n] ++ [((i,j+1), field!i!(j+1)) | j < n]
    let fieldCells = [((i, j), ListArrayGraphNode { nodeid = (i, j), incidenceList = neighbours i j}) | i <- [1..n], j <- [1..n]]
    let graph :: ListArrayGraph (Int, Int) Int = ArrayGraph $ array ((1, 1), (n, n)) fieldCells
    print $ field!1!1
    print $ filter (\((i, j), w) -> j == n && i == n) $ dijkstra graph (1::Int, 1::Int) (0::Int)

