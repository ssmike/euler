digits 0 _ = []
digits n base = n`mod`base : digits (n`div`base) base

numbers = 0:concatMap (\s -> reverse $ digits s 10) [1..]

indices = take 7 $ iterate (*10) 1

main = do
    print indices
    let digits = map (numbers!!) indices
    print digits
    print $ product digits
