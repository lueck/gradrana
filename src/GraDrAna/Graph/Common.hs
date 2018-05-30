{-# LANGUAGE RankNTypes #-}

module GraDrAna.Graph.Common
  ( Mappy
  ,  mkMapTuples
  , foldTuples
  , foldTuplesStage1
  , rmLoops
  , undirected
  ) where

import qualified Data.Map as Map
import Data.List
import Control.Lens

import GraDrAna.TypeDefs


-- * Generating graph data from '[[Turns]]'

-- | 'mkMapTuples' and 'foldTuples' are the workhorses of generating
-- data for graphs. These functions are very generic and can be reused
-- for different approaches.

-- | See tests for example usage.

-- | Abstract type alias for abstract helper functions
type Mappy k a = (Ord k) => Map.Map k (Map.Map k a)

mkMapTuples
  :: (Eq k, Ord k)
  => (b -> k) -- ^ Function for getting a key out of the list elements
  -> (b -> a) -- ^ Function to get a (initial) value of the map entry.
  -> [[b]] -- ^ a list of lists (e.g. of mapping keys)
  -> [[(k, Map.Map k a)]]
mkMapTuples getKey getVal lls =
  map (\l -> map (\el -> ( (getKey el)
                         , Map.fromList (map (\o -> (getKey o, getVal el)) l)
                         )) l) lls

-- | Stage 1 folding of the result of 'mkMapTuples'. This folds the
-- mappings of a time slice (a scene) into a mapping of mappings.
foldTuplesStage1
  :: Ord k
  => (a -> a -> a) -- ^ function for combining map values
  -> [(k, Map.Map k a)] -- ^ the result of 'mkMapTuples'
  -> Mappy k a
foldTuplesStage1 comb tups =
  foldl
  (\reg tup -> Map.insertWith (Map.unionWith comb) (fst tup) (snd tup) reg)
  Map.empty
  tups

-- | Stage 2 (and stage 1) folding of the result of
-- 'mkMapTuples'. This folds the mappings of all time slices into a
-- single nested mapping representing the whole play.
foldTuples
  :: Ord k
  => (a -> a -> a) -- ^ function for combining map values in stage 1
  -> (a -> a -> a) -- ^ function for combining map values in stage 2
  -> [[(k, Map.Map k a)]] -- ^ the list of lists generated by 'mkMapTuples'
  -> Mappy k a
foldTuples combS1 combS2 tups =
  foldl (Map.unionWith (Map.unionWith combS2)) Map.empty $
  map (foldTuplesStage1 combS1) tups


-- * Clean up registry of persons

-- | Remove edges to the same person, i.e. circles with a path length
-- of 1 from the graph.
rmLoops :: Persons -> Persons
rmLoops reg =
  Map.mapWithKey rmLoop reg
  where
    rmLoop k pers = pers & person_edgesTo %~ (Map.filterWithKey (\k' _ -> k' /= k))

-- | Remove edges, so that only one edge between two persons is left.
undirected :: Persons -> Persons
undirected reg = Map.foldlWithKey' mkReg Map.empty reg
  where
    mkReg acc k pers = Map.insert k (mkPers acc k pers) acc
    mkPers mp k pers =
      pers & person_edgesTo %~ (Map.filterWithKey (\k' _ -> not $ Map.member k' mp))
