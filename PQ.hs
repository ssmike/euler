module PQ (makeHeap, mergeHeaps, getMin, extractMin, BinHeap(EmptyHeap), checkHeapIntegrity, heapSize) where

data BinNode valType = BinNode {_nodeValue :: !valType, children :: ![BinNode valType]}
    deriving Show
data BinHeap valType = BinHeap {nodes :: ![(BinNode valType, Int)], _minVal :: !valType, _heapSize :: !Int} | EmptyHeap
    deriving Show

makeHeap x = BinHeap {  _minVal = x, nodes = [(BinNode{ _nodeValue = x, children = [] }, 1)], _heapSize = 1 }

_mergeNodes a b = if _nodeValue a < _nodeValue b
                    then BinNode{ _nodeValue = _nodeValue a, children = b: children a }
                    else BinNode{ _nodeValue = _nodeValue b, children = a: children b }

heapSize EmptyHeap = 0
heapSize x = _heapSize x

mergeHeaps EmptyHeap x = x
mergeHeaps x EmptyHeap = x
mergeHeaps x y = BinHeap { _minVal = _findMin resultList, nodes = resultList, _heapSize = heapSize x + heapSize y }
    where
        resultList = mergeNodeLists xs ys
        xs = nodes x
        ys = nodes y

mergeNodeLists xs ys = addLists Nothing xs ys
    where
        mergePairs :: Ord valType => (BinNode valType, Int) -> (BinNode valType, Int) -> (BinNode valType, Int)
        mergePairs x y = (_mergeNodes (fst x) (fst y), snd x + snd y)

        addLists :: Ord valType => Maybe (BinNode valType, Int) -> [(BinNode valType, Int)] -> [(BinNode valType, Int)] -> [(BinNode valType, Int)]
        addLists (Just x) xs ys = addLists Nothing (addOne x xs) ys
        addLists Nothing [] ys = ys
        addLists Nothing xs [] = xs
        addLists Nothing allx@(x:xs) ally@(y:ys)
            | snd x > snd y = y: addLists Nothing allx ys
            | snd x < snd y = x: addLists Nothing xs ally
            | otherwise = addLists (Just (mergePairs x y)) xs ys

        addOne :: Ord valType => (BinNode valType, Int) -> [(BinNode valType, Int)] -> [(BinNode valType, Int)]
        addOne y [] = [y]
        addOne y allx@(x:xs)
            | snd y < snd x = y: allx 
            | otherwise = addOne (mergePairs y x)  xs


getMin EmptyHeap = error "trying to get min from empty heap"
getMin x = _minVal x


_findMin xs = minimum $ map (_nodeValue . fst) xs

_toHeap [] = EmptyHeap
_toHeap xs = BinHeap { nodes = xs, _minVal = _findMin xs, _heapSize = sum $ map snd xs }

extractMin heap = _toHeap resultNodes
    where
        resultNodes = mergeNodeLists withoutMin $ reverse (zip (children minNode) (drop 1 $ iterate (`div`2) minSize))
        withoutMin = filter ((/=minSize) . snd) (nodes heap)
        (minNode, minSize) = head (filter ((==_minValue) . _nodeValue . fst) (nodes heap))
        _minValue = _findMin $ nodes heap


_checkNodeIntegrity :: Ord valType => valType -> Int -> BinNode valType -> Bool
_checkNodeIntegrity bound size node = _nodeValue node >= bound && and childrenChecks && trivialSizeCheck
    where 
        childrenChecks = [ _nodeValue node >= bound && _checkNodeIntegrity (_nodeValue node) size node | (node, size) <- zip (children node) (drop 1 $ iterate (`div`2) size)]
        trivialSizeCheck = size == 1 || not (emptyList (children node))

        emptyList [] = True
        emptyList _ = False

checkHeapIntegrity :: Ord valType => Int -> BinHeap valType -> Bool
checkHeapIntegrity size EmptyHeap = size == 0
checkHeapIntegrity size heap = minValueCheck && and nodeChecks && sizeCheck
    where
        nodeChecks = [_checkNodeIntegrity (_nodeValue node) size node | (node, size) <- nodes heap]
        minValueCheck = compare minValue (_minVal heap) == EQ
        minValue = _findMin $ nodes heap
        sizeCheck = size == sum (map snd $ nodes heap) && size == _heapSize heap
