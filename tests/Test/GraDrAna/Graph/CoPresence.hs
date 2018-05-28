{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.GraDrAna.Graph.CoPresence where

import Test.Framework
import qualified Data.Map as Map

import GraDrAna.Graph.CoPresence
import GraDrAna.TypeDefs

sampleFile = "doc/examples/lessing_emilia_1772.TEI-P5.xml"

oneListOfPersonIds :: [[PersonId]]
oneListOfPersonIds = [["A", "B", "C", "A", "C", "B"]]

twoListsOfPersonIds :: [[PersonId]]
twoListsOfPersonIds = [ ["A", "B", "C", "A", "C", "B"]
                      , ["A", "D", "C", "E"]
                      ]

test_mkMapTuplesEmpty =
  assertEqual
  ([[]] :: [[(k, Map.Map k Int)]])
  (mkMapTuples 1 ([[]] :: [[PersonId]]))

test_mkMapTuples1List =
  assertEqual
  ([[("A", Map.fromList [("B", 1), ("C", 1)]),
     ("B", Map.fromList [("A", 1), ("C", 1)]),
     ("C", Map.fromList [("A", 1), ("B", 1)])]]
    :: [[(String, Map.Map String Int)]])
  (mkMapTuples 1 oneListOfPersonIds)

test_mkMapTuples2Lists =
  assertEqual
  ([ [("A", Map.fromList [("B", 1), ("C", 1)]),
      ("B", Map.fromList [("A", 1), ("C", 1)]),
      ("C", Map.fromList [("A", 1), ("B", 1)])]
   , [("A", Map.fromList [("C", 1), ("D", 1), ("E", 1)]),
      ("D", Map.fromList [("A", 1), ("C", 1), ("E", 1)]),
      ("C", Map.fromList [("A", 1), ("D", 1), ("E", 1)]),
      ("E", Map.fromList [("A", 1), ("C", 1), ("D", 1)])]]
    :: [[(String, Map.Map String Int)]])
  (mkMapTuples 1 twoListsOfPersonIds)

test_foldTuplesEmpty =
  assertEqual
  Map.empty
  (foldTuples (+) $ mkMapTuples 1 ([[]] :: [[PersonId]]))

test_foldTuples1List =
  assertEqual
  (Map.fromList
   [("A", Map.fromList [("B", 1), ("C", 1)]),
    ("B", Map.fromList [("A", 1), ("C", 1)]),
    ("C", Map.fromList [("A", 1), ("B", 1)])])
  (foldTuples (+) $ mkMapTuples 1 oneListOfPersonIds)

test_foldTuples2Lists =
  assertEqual
  (Map.fromList
   [("A", Map.fromList [("B", 1), ("C", 2), ("D", 1), ("E", 1)]),
    ("B", Map.fromList [("A", 1), ("C", 1)]),
    ("C", Map.fromList [("A", 2), ("B", 1), ("D", 1), ("E", 1)]),
    ("D", Map.fromList [("A", 1), ("C", 1), ("E", 1)]),
    ("E", Map.fromList [("A", 1), ("C", 1), ("D", 1)])])
   (foldTuples (+) $ mkMapTuples 1 twoListsOfPersonIds)
