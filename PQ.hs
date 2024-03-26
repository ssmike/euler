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

mergeNodeLists xs ys = addLists Nothing xs ys
    where
        empty [] = True
        empty _ = False

        mergePairs :: Ord valType => (BinNode valType, Int) -> (BinNode valType, Int) -> (BinNode valType, Int)
        mergePairs x y = (mergeNodes (fst x) (fst y), snd x + snd y)

        addLists :: Ord valType => Maybe (BinNode valType, Int) -> [(BinNode valType, Int)] -> [(BinNode valType, Int)] -> [(BinNode valType, Int)]
        addLists (Just x) xs ys = addLists Nothing (addOne x xs) ys
        addLists Nothing [] ys = ys
        addLists Nothing xs [] = xs
        addLists Nothing allx@(x:xs) ally@(y:ys)
            | snd x > snd y = y:allx
            | snd x < snd y = x:ally
            | otherwise = addLists (Just (mergePairs x y)) xs ys

        addOne :: Ord valType => (BinNode valType, Int) -> [(BinNode valType, Int)] -> [(BinNode valType, Int)]
        addOne y [] = [y]
        addOne y allx@(x:xs)
            | snd y < snd x = y: allx 
            | otherwise = addOne (mergePairs y x)  xs

findMin xs = minimum $ map (nodeValue . fst) xs

getMin = minVal

extractMin :: Ord a => BinHeap a -> BinHeap a
extractMin heap = toHeap resultNodes
    where
        resultNodes = mergeNodeLists withoutMin (zip (children minNode) (drop 1 $ iterate (`div`2) minSize))
        withoutMin = filter ((/=minSize) . snd) (nodes heap)
        (minNode, minSize) = head (filter ((==minValue) . nodeValue . fst) (nodes heap))
        minValue = findMin $ nodes heap


checkNodeIntegrity bound node = 
checkHeapIntegrity heap = 
    where
