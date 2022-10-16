import Euler (digits)

numbers = 0:concatMap (`digits`10) [1..]

indices = take 7 $ iterate (*10) 1

main = do
    print indices
    let digits = map (numbers!!) indices
    print digits
    print $ product digits
