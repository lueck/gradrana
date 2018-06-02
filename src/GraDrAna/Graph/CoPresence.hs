{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

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

-- | An arrow that generates a GraphML graph from the edges given in
-- the registry of 'Persons'.
copresenceGraphmlArr :: (ArrowXml a) => Persons -> a XmlTree XmlTree
copresenceGraphmlArr reg =
  (mkqelem
    (mkNsName "graph" graphmlNs)
    [ -- attributes
      (sattr "id" "G")
    , (sattr "edgedefault" "undirected")
    ]
    -- child nodes
    (vertices ++ edges)
  )
  where
    vertices = map (uncurry mkVertice) $ Map.toAscList reg
    --mkVertice :: PersonId -> Person -> a XmlTree XmlTree
    mkVertice k pers =
      mkqelem
      (mkNsName "node" graphmlNs)
      [ (sattr "id" k) ]
      [ (mkqelem
         (mkNsName "data" graphmlNs)
         [ (sattr "key" nodeLabelEl) ]
         [ (txt $ fromMaybe "" $ _person_role pers)]
        )]
    edges = concat $ map (uncurry mkEdges) $ Map.toAscList $ undirected $ rmLoops reg
    mkEdges k pers = map (uncurry (mkEdge k)) $ Map.toAscList $ _person_edgesTo pers
    mkEdge from to label =
      mkqelem
      (mkNsName "edge" graphmlNs)
      [ (sattr "id" $ from ++ "-" ++ to)
      , (sattr "source" from)
      , (sattr "target" to)
      ]
      [ (mkqelem
         (mkNsName "data" graphmlNs)
         [ (sattr "key" edgeWeightEl) ]
         [ (txt $ show $ fromMaybe 0 $ _edgelabel_copresence label)]
        )]


-- | Generate a GraphML representation of a play.
copresenceGraphmlWriter :: Persons -> [[Turn]] -> App [Int]
copresenceGraphmlWriter reg _ = runGraphmlWriter (copresenceGraphmlArr reg)
