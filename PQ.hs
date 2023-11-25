module PQ (makeHeap, mergeHeaps, getMin, extractMin, emptyHeap) where

data BinNode valType = BinNode {nodeValue :: !valType, children :: ![BinNode valType]}
data BinHeap valType = BinHeap {nodes :: ![(BinNode valType, Int)], minVal :: !valType, heapSize :: Int}

makeHeap x = BinHeap {  minVal = x, nodes = [(BinNode{ nodeValue = x, children = [] }, 1)], heapSize = 1 }

mergeNodes a b = if nodeValue a < nodeValue b
                    then BinNode{ nodeValue = nodeValue a, children = b: children a }
                    else BinNode{ nodeValue = nodeValue b, children = a: children b }

emptyHeap = BinHeap []

toHeap xs = BinHeap { nodes = xs, minVal = findMin xs, heapSize = 1 }

mergeHeaps x y = BinHeap { minVal = findMin resultList, nodes = resultList, heapSize = heapSize x + heapSize y }
    where
        resultList = mergeNodeLists xs ys
        xs = nodes x
        ys = nodes y

mergeNodeLists xs ys = normalize (calcBounds xs ys) $ mergeLists xs ys 
    where
        empty [] = True
        empty _ = False

        normalize :: Ord valType => Int -> [(BinNode valType, Int)] -> [(BinNode valType, Int)]
        normalize _ [] = []
        normalize _ [x] = [x]
        normalize uniqBound all@(x:y:rest)
            | snd x /= snd y = if snd x >= uniqBound then all else x:normalize uniqBound (y:rest)
            | not (empty rest) && snd (head rest) == snd x = x : normalize uniqBound (y:rest)
            | otherwise = normalize uniqBound $ (mergeNodes (fst x) (fst y), snd x + snd y) : rest

        mergeLists [] x = x
        mergeLists x [] = x
        mergeLists allx@(x:xs) ally@(y:ys)
            | snd x >= snd y = y: mergeLists allx ys 
            | otherwise = x: mergeLists xs ally

--mergeNodeLists xs ys = normalize (calcBounds xs ys) $ mergeLists xs ys 
--    where
--        empty [] = True
--        empty _ = False
--
--        normalize :: Ord valType => Int -> [(BinNode valType, Int)] -> [(BinNode valType, Int)]
--        normalize _ [] = []
--        normalize _ [x] = [x]
--        normalize uniqBound all@(x:y:rest)
--            | snd x /= snd y = if snd x >= uniqBound then all else x:normalize uniqBound (y:rest)
--            | not (empty rest) && snd (head rest) == snd x = x : normalize uniqBound (y:rest)
--            | otherwise = normalize uniqBound $ (mergeNodes (fst x) (fst y), snd x + snd y) : rest
--
--        mergeLists [] x = x
--        mergeLists x [] = x
--        mergeLists allx@(x:xs) ally@(y:ys)
--            | snd x >= snd y = y: mergeLists allx ys 
--            | otherwise = x: mergeLists xs ally

findMin xs = minimum $ map (nodeValue . fst) xs

getMin xs = minVal

extractMin :: Ord a => BinHeap a -> BinHeap a
extractMin heap = toHeap $ resultNodes
    where
        resultNodes = mergeNodeLists withoutMin (zip (children minNode) (iterate (`div`2) minSize))
        withoutMin = filter ((/=minValue) . nodeValue . fst) (nodes heap)
        (minNode, minSize) = head (filter ((==minValue) . nodeValue . fst) (nodes heap))
        minValue = findMin $ nodes heap
