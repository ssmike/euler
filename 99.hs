import Data.List.Split (splitWhen)


main = do
    text <- splitWhen (=='\n') `fmap` readFile "/home/ssmike/Downloads/0099_base_exp.txt"
    let rows = map (splitWhen (==',')) text
    let vals = map (\[a, b] -> ((read b :: Float) * (log (read a :: Float)), a, b)) rows
    print $ take 4 rows --let lines = 
    print $ take 4 vals 
    print $ maximum $ zip vals [1..]
