digits 0 _ = []
digits n base = n`mod`base : digits (n`div`base) base

palyndrom x = x == reverse x 

check x = palyndrom (digits x 2) && palyndrom (digits x 10)

limit = 10^6

main = do
    print $ sum $ filter check [1..limit]
