{-# LANGUAGE FlexibleContexts #-}

-- | Construct a graph on the basis of whether two characters are both
-- present in at least one unit (time slice or scene or something
-- else). The resulting graph's edges do not have a direction. Each
-- edge's label represents the number of units (time slices), in which
-- the two persons are both present.

-- | Combined with the time slice splitter this implements the
-- approach of J. Stiller et al. 2003.

module GraDrAna.Graph.CoPresence
  ( copresence
  , copresencePure
  -- * graphml output
  , copresenceGraphmlArr
  , copresenceGraphmlWriter
  ) where

--import Data.Graph.Inductive
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Lens
import Text.XML.HXT.Core
import Data.Default.Class

import GraDrAna.App
import GraDrAna.TypeDefs
import GraDrAna.Graph.GraphML
import GraDrAna.Graph.Common

-- | Run 'copresence' in a monad with the 'Config' present. The
-- signature fits the data flow.
copresence :: AppConfig m => Persons -> [[Turn]] -> m (Persons, [[Turn]])
copresence reg turns = return ((copresencePure reg turns), turns)

-- | Calculate the co-present persons from the turns. This is the
-- workhorse of the construction of the copresence graph. The edges
-- are stored to the map of 'Person' records. It's not a graph, but
-- aggregate data for constructing the graph.
copresencePure :: Persons -> [[Turn]] -> Persons
copresencePure reg turns =
  toRegistry mkLabel reg $
  foldTuples const (+) $        -- for an explanation see tests
  mkMapTuples id (const 1) $
  speakers turns
  where
    mkLabel amount = def & edgelabel_copresence .~ (Just amount)

-- | Get the speakers from the turns.
speakers :: [[Turn]] -> [[PersonId]]
speakers turns = map getSpeakers turns
  where
    getSpeakers = map ((fromMaybe "UNKOWN") . _turn_roleId)


-- * Output to GraphML

-- | Generate a GraphML representation of a play.
copresenceGraphmlWriter :: Persons -> [[Turn]] -> App [Int]
copresenceGraphmlWriter reg _ =
  runGraphmlWriter
  (mkGraphmlGraph
   (undirected . rmLoops)
   (show . (fromMaybe 0) . (^.edgelabel_copresence))
   "undirected"
   reg)
