import Graph
import Data.Array (Ix, Array, range, bounds, (!), array, listArray)
import Data.List.Split (splitOn)

main = do
    file <- readFile "/home/ssmike/Downloads/0082_matrix.txt"
    let nums = map (map (read :: String -> Int) . splitOn ",") (lines file)
    let width :: Int = length nums
    let height :: Int = length $ head nums
    let field = listArray (1, width) [listArray (1, height) row | row <- nums]
    let firstColumn = [((i, 0), ListArrayGraphNode { nodeid = (i, 0), incidenceList = [((i+1, 0), 0) | i < height] ++ [((i, 1), field!i!1)] }) | i <- [1..width]]
    let neighbours i j = [((i-1,j), field!(i-1)!j) | i > 1] ++ [((i+1,j), field!(i+1)!j) | i < width] ++ [((i,j+1), field!i!(j+1)) | j < height] -- [((i,j-1), field!i!(j-1)) | j > 1] ++ 
    let fieldCells = [((i, j), ListArrayGraphNode { nodeid = (i, j), incidenceList = neighbours i j}) | i <- [1..width], j <- [1..height]]
    let graph :: ListArrayGraph (Int, Int) Int = ArrayGraph $ array ((1, 0), (width, height)) $ firstColumn ++ fieldCells
    print $ maximum $ map snd $ filter (\((i, j), w) -> j == height) $ dijkstra graph (1::Int, 0::Int) (0::Int)

