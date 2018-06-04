module GraDrAna.Graph.GraphML where

import Text.XML.HXT.Core
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Map as Map

import GraDrAna.App
import GraDrAna.TypeDefs


graphmlNs :: String
graphmlNs = "http://graphml.graphdrawing.org/xmlns"

graphmlSchemaLoc :: String
graphmlSchemaLoc = "http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd"

xsiNs :: String
xsiNs = "http://www.w3.org/2001/XMLSchema-instance"

nodeLabelEl :: String
nodeLabelEl = "d0"

nodeWeightEl :: String
nodeWeightEl = "d1"

edgeWeightEl :: String
edgeWeightEl = "d2"

-- | Write a GraphML representation of the play's graph to the output
-- file given in the config.
--
-- USAGE: See 'turnQuantityGraphmlWriter'.
--
-- We could do everything generically asking the config for getters of
-- the edge weights and other parameters. But that way, too much would
-- be done behind the scenes and it wouldn't be easy to test, but easy
-- to break things.
runGraphmlWriter ::
  IOSLA (XIOState ()) XmlTree XmlTree -- ^ arrow for making the graph element
  -> App [Int]
runGraphmlWriter body = do
  outFile <- asks _cfg_outFile
  -- Passing "-" to writeDocument means to write to stdout
  liftIO $ runX
    (mkGraphmlDoc body >>>
      writeDocument [withIndent yes] (fromMaybe "-" outFile) >>>
      getErrStatus)

mkGraphmlDoc :: (ArrowXml a)
  => a XmlTree XmlTree        -- ^ arrow for making the graph element
  -> a XmlTree XmlTree          -- ^ returns an xml arrow
mkGraphmlDoc graphEl =
  root                          -- make the hxt root node containing XML-Decl etc.
  []                            -- its attribute nodes
  [                             -- child nodes
    (mkqelem -- xml root element
      (mkQName "" "graphml" graphmlNs)
      [ -- attributes
        (sattr "xmlns" graphmlNs)
      , (sattr "xmlns:xsi" xsiNs)
      , (sattr "xsi:schemaLocation" $ graphmlNs ++ " " ++ graphmlSchemaLoc)
      ]
      (keys ++ [graphEl]) -- child nodes
    )
  ]
  where
    keys = [ (mkqelem
              (mkQName "" "key" graphmlNs)
              [ (sattr "id" nodeLabelEl)
              , (sattr "for" "node")
              , (sattr "attr.name" "label")
              , (sattr "attr.type" "string")
              ]
              [])
           , (mkqelem
              (mkQName "" "key" graphmlNs)
              [ (sattr "id" nodeWeightEl)
              , (sattr "for" "node")
              , (sattr "attr.name" "weight")
              , (sattr "attr.type" "double")
              ]
              [])
           , (mkqelem
              (mkQName "" "key" graphmlNs)
              [ (sattr "id" edgeWeightEl)
              , (sattr "for" "edge")
              , (sattr "attr.name" "weight")
              , (sattr "attr.type" "double")
              ]
              [])
           ]

-- | A generic function that creates an arrow for a GraphML
-- representation of the graph given in the registry of 'Persons'.
mkGraphmlGraph
  :: (ArrowXml a)
  => (Persons -> Persons) -- ^ Function to post-process edges,
                          -- e.g. remove circles of path length 1.
  -> (EdgeLabel -> String) -- ^ Function to get the edge weight from
                           -- the edge label
  -> String                -- ^ Graph Type, e.g. @"undirected"@.
  -> Persons               -- ^ the registry of persons
  -> a XmlTree XmlTree
mkGraphmlGraph filterEdges getEdgeWeight graphType reg =
  (mkqelem
    (mkNsName "graph" graphmlNs)
    [ -- attributes
      (sattr "id" "G")
    , (sattr "edgedefault" graphType)
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
    edges = concat $ map (uncurry mkEdges) $ Map.toAscList $ filterEdges reg
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
         [ (txt $ getEdgeWeight label)]
        )]
