import qualified Data.Map as M
import Data.Array (Ix, Array, range, bounds, (!))
import Data.List ()
import PQ

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

dijkstra :: (Num w, Graph i w g) => g -> i -> [(i, w)]
dijkstra graph start = 
    where
        seed = (M.empty, M.fromList [(start, 0)], makeHeap (0, start))
        step (distances, candidateDistances, candidates)
            | M.member minNode distances = skip
            | M.lookup minNode candidateDistances == Just minDistance = (updatedDistances, M.delete minNode candidateDistances, updatedCandidates)
            | otherwise = skip
            where
                nextHeap = extractMin candidates
                skip = (distances, candidates, nextHeap)

                (minDistance, minNode) = findMin candidates
                updatedDistances = M.insert minNode minDistance distances

                candidateUpdates = 
                updatedCandidates = 
                updateCandidateDistances = 

--data Node idtype edgetype  = Node {nodeid :: idtype, edges :: [edgetype]};
--class Ix i => Graph

main = do
    let heap = foldl1 mergeHeaps (map makeHeap [1..100])
    print $ findMin heap
    print $ findMin . extractMin $ heap

