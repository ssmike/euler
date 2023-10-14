import Euler (digits)
import Data.Ratio (numerator, denominator)

eexp = 2:1: from 1
    where
        from k  = (2*k):1:1: from (k + 1)

makeRatio exp = make $ reverse exp
    where
        make :: [Integer] -> Rational
        make [x] = fromIntegral x
        make (x:xs) = (fromIntegral x::Rational) + recip (make xs)

main = do
    print $ take 12 eexp
    print $ sum . (`digits` 10) . numerator . makeRatio $ take 10 eexp
    print $ sum . (`digits` 10) . numerator . makeRatio $ take 100 eexp
