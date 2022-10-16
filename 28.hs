diags = drop 1 $ scanl (+) 0 [1, 24, 76]

main = do
    print diags

