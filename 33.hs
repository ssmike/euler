import Control.Monad (when)
import qualified Data.List as L

uniqueList list = map head $ L.group $ L.sort list

fractions = uniqueList $ do
    nom <- filter filterNominator [10..99]
    denom <- filter filterNominator [10..99]

    opNom <- ops <*> [nom]
    opDenom <- ops <*> [denom]

    [(nom, denom) | nom * opDenom == opNom * denom, nom < denom, sumNum nom - opNom == sumNum denom - opDenom]
    where   
        ops :: [Int -> Int]
        ops = [(`div`10), (`mod`10)]

        sumNum x = x`div`10 + x`mod`10

        filterNominator x = x`mod`10 /=0

mulFractions (a,b) (c,d) = (nom `div` common, denom `div` common)
    where
        nom = a * c
        denom = b * d
        common = gcd nom denom

main = do
    print fractions
    print $ map (\(a, b) -> b `mod` a) fractions
    print $ scanl mulFractions (1, 1) fractions
    
