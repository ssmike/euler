module Graph (NodeCollection, Graph, ListArrayGraphNode(..), ListArrayGraph(..), dijkstra) where

import Data.Array (Ix, Array, range, bounds, (!), array, listArray)
import qualified Data.Map as M
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
    deriving Show
newtype ListArrayGraph i w = ListArrayGraph (Array i (ListArrayGraphNode i w))
    deriving Show

instance Ix i => NodeCollection i (ListArrayGraph i w) where
    nodes (ListArrayGraph a) = range $ bounds a

instance Ix i => Graph i w (ListArrayGraph i w) where
    edges (ListArrayGraph nodes) node = incidenceList $ nodes ! node


dijkstra :: (Ord i, Num w, Ord w, Graph i w g) => g -> i -> w -> [(i, w)]
dijkstra graph start zero = answer . head $ dropWhile (not.final) $ iterate step seed
    where
        answer (a, _) = M.assocs a
        final (distances, candidates) = (==0) $ PQ.heapSize candidates
        seed = (M.empty, PQ.makeHeap (zero, start))
        step (distances, candidates)
            | M.member minNode distances = skip
            | otherwise = (M.insert minNode minDistance distances, updatedCandidates)
            where
                nextHeap = PQ.extractMin candidates
                skip = (distances, nextHeap)

                (minDistance, minNode) = PQ.getMin candidates

                updates = [(minDistance + w, nodeid) | (nodeid, w) <- edges graph minNode]
                updatedCandidates = foldl1 PQ.mergeHeaps (map PQ.makeHeap updates ++ [nextHeap])
