import Euler (digits)

main = print $ maximum $ do
        a <- [1..99]
        b <- [1..99]
        [sum $ (a^b) `digits` 10]
