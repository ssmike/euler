import Data.Array (array, (!))
import Euler (phis)

main = do
    print $ phis 10
    print $ sum $ phis 4
    print $ sum $ phis 10
    print $ sum $ phis 8
    print $ sum $ phis 1000000
