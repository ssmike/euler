import Euler (phis, digits, makeNumber)
import Data.List (sort)
import Data.Array (array, (!))
import Control.Monad (forM_)

limit = 10^7
canonpermute x = makeNumber $ reverse $ sort $ digits x 10

allPhis = phis limit

totients :: [Integer]
totients = filter (\x -> canonpermute x == canonpermute (allPhis ! x)) [2..limit]

pairs :: [(Rational, Integer)]
pairs = map (\x -> ((toRational x) / (toRational (allPhis ! x)), x)) totients

main = do
    forM_ (scanl1 min pairs) print
    --print $ minimum pairs
