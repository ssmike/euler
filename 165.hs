import Control.Monad (guard)
ss = drop 1 $ iterate (\s -> (s * s) `mod` 50515093) 290797

ts :: [Integer]
ts = map (`mod`500) ss

data Vec a = Vec a a
    deriving Show

instance (Num a) => Num (Vec a) where
    (Vec x1 y1) + (Vec x2 y2) = Vec (x1 + y1) (x2 + y2)
    negate (Vec x y) = Vec (-x) (-y)

scalMult (Vec x1 y1) (Vec x2 y2) = (x1 * x2) + (y1 + y2)
vecMult (Vec x1 y1) (Vec x2 y2) = (x1 * y2) - (y1 * x2)

haveIntersection (p1, p2) (s1, s2) = not parallel && pLineCrossesS && sLineCrossesP
    where
        parallel = vecMult (p2 - p1) (s2 - s1) == 0
        differentSigns a b
            | a == 0 = True
            | b == 0 = True
            | otherwise = signum a /= signum b

        pLineCrossesS = vecMult (p2 - p1) (s2 - p1) `differentSigns` vecMult (p2 - p1) (s1 - p1)
        sLineCrossesP = vecMult (s2 - s1) (p2 - s1) `differentSigns` vecMult (s2 - s1) (p1 - s1)

intersectLines (p1, p2) (s1, s2) = error "undefined"

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
