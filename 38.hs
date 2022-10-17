import qualified Data.List as L
import Control.Monad (forM_)
import Euler (digits, uniqueList)

allPanDigitsFor n lim = concatMap (`digits` 10) $ (*) <$> [n] <*> [1..lim]
allPanDigits n = map (allPanDigitsFor n) [1..9]

panDigits n = head . dropWhile ((<9).length) $ allPanDigits n
panNumber n = foldl (\a b -> a * 10 + b) 0 $ panDigits n

check n = [1..9] == L.sort digits
    where digits = panDigits n

limit = 10^5

main = do
    let filtered = filter check [1..limit]
    let panNumbers = map panNumber filtered
    print panNumbers
    print $ maximum panNumbers
