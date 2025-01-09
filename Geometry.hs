module Geometry (Vec(Vec), scalMult, vecMult, haveIntersection, intersectLines) where

data Vec a = Vec a a
    deriving (Show, Eq, Ord)

instance (Num a) => Num (Vec a) where
    (Vec x1 y1) + (Vec x2 y2) = Vec (x1 + x2) (y1 + y2)
    negate (Vec x y) = Vec (-x) (-y)

instance Functor Vec where
    fmap func (Vec first second) = Vec (func first) (func second) 

scale (Vec x y) alpha = Vec (x * alpha) (y * alpha)
scalMult (Vec x1 y1) (Vec x2 y2) = (x1 * x2) + (y1 + y2)
vecMult (Vec x1 y1) (Vec x2 y2) = (x1 * y2) - (y1 * x2)

haveIntersection (p1, p2) (s1, s2) = not parallel && pLineCrossesS && sLineCrossesP
    where
        parallel = vecMult (p2 - p1) (s2 - s1) == 0
        differentSigns a b
            | a == 0 = False
            | b == 0 = False
            | otherwise = signum a /= signum b

        pLineCrossesS = vecMult (p2 - p1) (s2 - p1) `differentSigns` vecMult (p2 - p1) (s1 - p1)
        sLineCrossesP = vecMult (s2 - s1) (p2 - s1) `differentSigns` vecMult (s2 - s1) (p1 - s1)

intersectLines :: (Vec Integer, Vec Integer) -> (Vec Integer, Vec Integer) -> Vec Rational
intersectLines (p1, p2) (s1, s2) = p1r + scale v1r alpha
    where
        p1r = fmap toRational p1
        v1 = p2 - p1
        v1r = fmap toRational v1
        v2 = s2 - s1
        alpha = (toRational $ vecMult (s1 - p1) v2) / (toRational $ vecMult v1 v2)
