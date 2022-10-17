import Control.Monad (forM_)
import Euler (intersectSorted)


triangles = map (\n -> (n * (n + 1)) `div` 2) [1..]
pentagonals = map (\n -> (n * (3 * n - 1)) `div` 2) [1..]
hexagonals = map (\n -> n * (2*n - 1)) [1..]


intersect3 a b c = intersectSorted a $ intersectSorted b c 

main = forM_ (intersect3 triangles pentagonals hexagonals) print 
