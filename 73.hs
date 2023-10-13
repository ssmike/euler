import Euler (multiples)


bounds denom = (denom `div` 3 + 1, (denom - 1) `div` 2)
allNoms denom = filter (\nom -> all (\mul -> nom `mod` mul /= 0) tocheck) [first..last]
    where
        tocheck = map fst $ multiples denom
        (first,last) = bounds denom

allFracs lim = do
    denom <- [2..lim]
    nom <- allNoms denom
    return (nom, denom)

main = do
    print $ bounds 12
    print $ multiples 12
    print $ allNoms 12
    print $ allFracs 8
    print $ length $ allFracs 12000
