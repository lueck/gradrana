{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-- | Construct a graph on the basis of whether two characters are both
-- present in at least one unit (time slice or scene or something
-- else). The resulting graph's edges do neither have a direction nor
-- a label.

-- | Combined with the time slice splitter this implements the
-- approach of J. Stiller et al. 2003.

module GraDrAna.Graph.CoPresence
  ( copresenceIO
  , copresence
  -- * helper functions
  , mkMapTuples
  , foldTuples
  ) where

--import Data.Graph.Inductive
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Lens

import GraDrAna.TypeDefs

-- | Abstract type alias for abstract helper functions
type Mappy k a = (Ord k) => Map.Map k (Map.Map k a)

-- | Intermediate data structure for co-present persons of the
-- play. It's a map of maps, the outer representing the persons (ids
-- only) in the play and the inner representing the edges to other
-- persons.
type CoMap = Mappy PersonId Int

-- | Run 'copresence' in the IO monad. The signature fits the data
-- flow.
copresenceIO :: Persons -> [[Turn]] -> IO (Persons, [[Turn]])
copresenceIO reg turns = return ((copresence reg turns), turns) 

-- | Calculate the co-present persons from the turns. This is the
-- workhorse of the construction of the copresence graph. The edges
-- are stored to the map of 'Person' records. It's not a graph, but
-- aggregate data for constructing the graph.
copresence :: Persons -> [[Turn]] -> Persons
copresence reg turns = toRegistry reg $ foldTuples $ mkMapTuples $ speakers turns

-- | Get the speakers from the turns.
speakers :: [[Turn]] -> [[PersonId]]
speakers turns = map getSpeakers turns
  where
    getSpeakers = map ((fromMaybe "UNKOWN") . _turn_roleId)

-- | Feed 'CoMap' into the registry of persons.
toRegistry :: Persons -> CoMap -> Persons
toRegistry reg cs =
  Map.mapWithKey
  (\k p -> p & person_edgesTo .~ (fromMaybe Map.empty $ Map.lookup k cs))
  reg  

foldTuples :: Ord k => [[(k, Map.Map k a)]] -> Mappy k a
foldTuples tups = foldl (Map.unionWith Map.union) Map.empty $ map foldTuples' tups

foldTuples' :: Ord k => [(k, Map.Map k a)] -> Mappy k a
foldTuples' tups =
  foldl
  (\reg tup -> Map.insertWith Map.union (fst tup) (snd tup) reg)
  Map.empty
  tups

mkTuples :: (Eq a) => [[a]] -> [[(a, [a])]]
mkTuples = map ((\l -> map (\p -> (p, delete p l)) l) . nub)

mkMapTuples :: (Eq k, Ord k) => [[k]] -> [[(k, Map.Map k Int)]]
mkMapTuples = map ((\l -> map (\p -> (p, Map.fromList (map (,1) $ delete p l))) l) . nub)
