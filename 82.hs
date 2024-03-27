import qualified Data.Map as M
import Data.Array (Ix, Array, range, bounds, (!))
import qualified PQ

class Eq nodeid => NodeCollection nodeid a where
    nodes :: a -> [nodeid]

class NodeCollection nodeid a => Graph nodeid edgemeta a where
    edges :: a -> nodeid -> [(nodeid, edgemeta)]
    edges graph node = do
        neighbour <- nodes graph
        let e = edge graph node neighbour 
        meta <- unwrapMaybe e
        return (neighbour, meta)
        where
            unwrapMaybe (Just x) = [x]
            unwrapMaybe Nothing = []

    edge :: a -> nodeid -> nodeid -> Maybe edgemeta
    edge graph a b = if isempty foundEdges
                        then Nothing
                        else let [(_, meta)] = foundEdges in Just meta
        where 
            foundEdges = filter ((==b) . fst) $ edges graph a

            isempty [] = True
            isempty _ = False

data ListArrayGraphNode i w = ListArrayGraphNode {nodeid :: i, incidenceList :: [(i, w)]}
newtype ListArrayGraph i w = ArrayGraph (Array i (ListArrayGraphNode i w))

instance Ix i => NodeCollection i (ListArrayGraph i w) where
    nodes (ArrayGraph a) = range $ bounds a

instance Ix i => Graph i w (ListArrayGraph i w) where
    edges (ArrayGraph nodes) node = incidenceList $ nodes ! node


dijkstra :: (Ord i, Ord w, Num w, Graph i w g) => g -> i -> [(i, w)]
dijkstra graph start = answer . head $ dropWhile (not.final) $ iterate step seed
    where
        answer (a, _) = M.assocs a
        final (distances, candidates) = (==0) $ PQ.heapSize candidates
        seed = (M.empty, PQ.makeHeap (0, start))
        step (distances, candidates)
            | M.member minNode distances = skip
            | otherwise = (M.insert minNode minDistance distances, updatedCandidates)
            where
                nextHeap = PQ.extractMin candidates
                skip = (distances, nextHeap)

                (minDistance, minNode) = PQ.getMin candidates

                updates = [(minDistance + w, nodeid) | (nodeid, w) <- edges graph minNode]
                updatedCandidates = foldl1 PQ.mergeHeaps (map PQ.makeHeap updates ++ [nextHeap])

--data Node idtype edgetype  = Node {nodeid :: idtype, edges :: [edgetype]};
--class Ix i => Graph

main = do
    let heap = foldl1 PQ.mergeHeaps (map PQ.makeHeap [1..100])
    print $ PQ.getMin heap
    print $ PQ.getMin . PQ.extractMin $ heap

