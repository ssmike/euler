import Control.Monad (forM_)


triangles = map (\n -> (n * (n + 1)) `div` 2) [1..]
pentagonals = map (\n -> (n * (3 * n - 1)) `div` 2) [1..]
hexagonals = map (\n -> (n * (2*n - 1))) [1..]

intersect [] xs = []
intersect (x:xs) (y:ys)
    | x > y = intersect (y:ys) (x:xs)
    | x < y = intersect xs (y:ys)
    | otherwise = x:intersect xs ys


intersect3 a b c = intersect a $ intersect b c 

main = forM_ (intersect3 triangles pentagonals hexagonals) print 
