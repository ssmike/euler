import Control.Monad (guard)
import Euler (uniqueList)
import Geometry (haveIntersection, Vec(Vec), intersectLines)

ss = drop 1 $ iterate (\s -> (s * s) `mod` 50515093) 290797

ts :: [Integer]
ts = map (`mod`500) ss

pointPairs = foldSegments $ foldPoints ts 
    where
        foldSegments pts = (p1, p2) : foldSegments (drop 2 pts)
            where
                [p1, p2] = take 2 pts

        foldPoints ts = Vec x y : foldPoints (drop 2 ts)
            where
                [x, y] = take 2 ts


explorePairs pts = do
    first <- pts
    second <- pts
    guard $ first /= second
    guard $ haveIntersection first second
    return $ intersectLines first second


main = do
    let segments = take 5000 pointPairs
    print $ take 1 pointPairs

    let (l1, l2, l3) = ((Vec 27 44, Vec 12 32), (Vec 46 53, Vec 17 62), (Vec 46 70, Vec 22 40))
    print (l1, l2, l3)
    print $ haveIntersection l1 l3
    print $ haveIntersection l2 l3
    print $ haveIntersection (Vec 0 1, Vec 1 0) (Vec 0 0, Vec 1 1)

    print $ length $ uniqueList $ explorePairs [l1, l2, l3]
    print $ length $ uniqueList $ explorePairs segments
