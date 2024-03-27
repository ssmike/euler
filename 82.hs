import Graph
import Data.Array (Ix, Array, range, bounds, (!), array, listArray)
import Data.List.Split (splitOn)

main = do
    file <- readFile "/home/ssmike/Downloads/0082_matrix.txt"
    let nums = map (map (read :: String -> Int) . splitOn ",") (lines file)
    let n = length nums
    let field = listArray (1, n) [listArray (1, n) row | row <- nums]
    let firstColumn = [((i, 0), ListArrayGraphNode { nodeid = (i, 0), incidenceList = [((i+1, 0), 0) | i < n] ++ [((i, 1), field!i!1)] }) | i <- [1..n]]
    let neighbours i j = [((i-1,j), field!(i-1)!j) | i > 1] ++ [((i+1,j), field!(i+1)!j) | i < n] ++ [((i,j+1), field!i!(j+1)) | j < n] -- [((i,j-1), field!i!(j-1)) | j > 1] ++ 
    let fieldCells = [((i, j), ListArrayGraphNode { nodeid = (i, j), incidenceList = neighbours i j}) | i <- [1..n], j <- [1..n]]
    let graph :: ListArrayGraph (Int, Int) Int = ListArrayGraph $ array ((1, 0), (n, n)) $ firstColumn ++ fieldCells
    print $ minimum $ map snd $ filter (\((i, j), w) -> j == n) $ dijkstra graph (1::Int, 0::Int) (0::Int)
