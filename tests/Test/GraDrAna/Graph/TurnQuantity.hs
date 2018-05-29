{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.GraDrAna.Graph.TurnQuantity where

import Test.Framework
import qualified Data.Map as Map
import Control.Lens
import Data.Default.Class

import GraDrAna.Graph.Common
import GraDrAna.Graph.TurnQuantity
import GraDrAna.TypeDefs


turnSample1 :: [[Turn]]
turnSample1 =
  [[ def
     & turn_roleId .~ (Just "A")
     & turn_turn .~ (Just "Hello World")
   , def
     & turn_roleId .~ (Just "B")
     & turn_turn .~ (Just "Hello A!")
   , def
     & turn_roleId .~ (Just "A")
     & turn_turn .~ (Just "Hello B!")
   , def
     & turn_roleId .~ (Just "C")
     & turn_turn .~ (Just "Hello!")
   ]]

turnSample1' = quantities turnSample1

turnSample2 :: [[Turn]]
turnSample2 =
  [[ def
     & turn_roleId .~ (Just "A")
     & turn_turn .~ (Just "Hello World")
   , def
     & turn_roleId .~ (Just "B")
     & turn_turn .~ (Just "Hello A!")
   , def
     & turn_roleId .~ (Just "A")
     & turn_turn .~ (Just "Hello B!")
   , def
     & turn_roleId .~ (Just "C")
     & turn_turn .~ (Just "Hello!")
   ]
  ,[ def
     & turn_roleId .~ (Just "A")
     & turn_turn .~ (Just "Hello second World")
   , def
     & turn_roleId .~ (Just "D")
     & turn_turn .~ (Just "Hello A!")
   , def
     & turn_roleId .~ (Just "A")
     & turn_turn .~ (Just "Hello D!")
   , def
     & turn_roleId .~ (Just "D")
     & turn_turn .~ (Just "Hello!")
   ]
  ]

turnSample2' = quantities turnSample2

test_mkMapTuples0 =
  assertEqual
  ([[]] :: [[(k, Map.Map k TurnQuantity)]])
  (mkMapTuples _turnQuant_roleId id ([[]] :: [[TurnQuantity]]))

test_stage1sample1 =
  assertEqual
  [Map.fromList
   [("A",
     Map.fromList
       [("A",
         TurnQuantity{_turnQuant_roleId = "A", _turnQuant_turns = 2,
                      _turnQuant_chars = 19, _turnQuant_words = 4,
                      _turnQuant_lines = 2}),
        ("B",
         TurnQuantity{_turnQuant_roleId = "A", _turnQuant_turns = 2,
                      _turnQuant_chars = 19, _turnQuant_words = 4,
                      _turnQuant_lines = 2}),
        ("C",
         TurnQuantity{_turnQuant_roleId = "A", _turnQuant_turns = 2,
                      _turnQuant_chars = 19, _turnQuant_words = 4,
                      _turnQuant_lines = 2})]),
    ("B",
     Map.fromList
       [("A",
         TurnQuantity{_turnQuant_roleId = "B", _turnQuant_turns = 1,
                      _turnQuant_chars = 8, _turnQuant_words = 2, _turnQuant_lines = 1}),
        ("B",
         TurnQuantity{_turnQuant_roleId = "B", _turnQuant_turns = 1,
                      _turnQuant_chars = 8, _turnQuant_words = 2, _turnQuant_lines = 1}),
        ("C",
         TurnQuantity{_turnQuant_roleId = "B", _turnQuant_turns = 1,
                      _turnQuant_chars = 8, _turnQuant_words = 2,
                      _turnQuant_lines = 1})]),
    ("C",
     Map.fromList
       [("A",
         TurnQuantity{_turnQuant_roleId = "C", _turnQuant_turns = 1,
                      _turnQuant_chars = 6, _turnQuant_words = 1, _turnQuant_lines = 1}),
        ("B",
         TurnQuantity{_turnQuant_roleId = "C", _turnQuant_turns = 1,
                      _turnQuant_chars = 6, _turnQuant_words = 1, _turnQuant_lines = 1}),
        ("C",
         TurnQuantity{_turnQuant_roleId = "C", _turnQuant_turns = 1,
                      _turnQuant_chars = 6, _turnQuant_words = 1,
                      _turnQuant_lines = 1})])]]
  (map (foldTuplesStage1 addQuant) $ mkMapTuples _turnQuant_roleId id turnSample1')

test_stage1sample2 =
  assertEqual
  [Map.fromList
   [("A",
     Map.fromList
       [("A",
         TurnQuantity{_turnQuant_roleId = "A", _turnQuant_turns = 2,
                      _turnQuant_chars = 19, _turnQuant_words = 4,
                      _turnQuant_lines = 2}),
        ("B",
         TurnQuantity{_turnQuant_roleId = "A", _turnQuant_turns = 2,
                      _turnQuant_chars = 19, _turnQuant_words = 4,
                      _turnQuant_lines = 2}),
        ("C",
         TurnQuantity{_turnQuant_roleId = "A", _turnQuant_turns = 2,
                      _turnQuant_chars = 19, _turnQuant_words = 4,
                      _turnQuant_lines = 2})]),
    ("B",
     Map.fromList
       [("A",
         TurnQuantity{_turnQuant_roleId = "B", _turnQuant_turns = 1,
                      _turnQuant_chars = 8, _turnQuant_words = 2, _turnQuant_lines = 1}),
        ("B",
         TurnQuantity{_turnQuant_roleId = "B", _turnQuant_turns = 1,
                      _turnQuant_chars = 8, _turnQuant_words = 2, _turnQuant_lines = 1}),
        ("C",
         TurnQuantity{_turnQuant_roleId = "B", _turnQuant_turns = 1,
                      _turnQuant_chars = 8, _turnQuant_words = 2,
                      _turnQuant_lines = 1})]),
    ("C",
     Map.fromList
       [("A",
         TurnQuantity{_turnQuant_roleId = "C", _turnQuant_turns = 1,
                      _turnQuant_chars = 6, _turnQuant_words = 1, _turnQuant_lines = 1}),
        ("B",
         TurnQuantity{_turnQuant_roleId = "C", _turnQuant_turns = 1,
                      _turnQuant_chars = 6, _turnQuant_words = 1, _turnQuant_lines = 1}),
        ("C",
         TurnQuantity{_turnQuant_roleId = "C", _turnQuant_turns = 1,
                      _turnQuant_chars = 6, _turnQuant_words = 1,
                      _turnQuant_lines = 1})])]
   ,Map.fromList
   [("A",
     Map.fromList
       [("A",
         TurnQuantity{_turnQuant_roleId = "A", _turnQuant_turns = 2,
                      _turnQuant_chars = 26, _turnQuant_words = 5,
                      _turnQuant_lines = 2}),
        ("D",
         TurnQuantity{_turnQuant_roleId = "A", _turnQuant_turns = 2,
                      _turnQuant_chars = 26, _turnQuant_words = 5,
                      _turnQuant_lines = 2})]),
    ("D",
     Map.fromList
       [("A",
         TurnQuantity{_turnQuant_roleId = "D", _turnQuant_turns = 2,
                      _turnQuant_chars = 14, _turnQuant_words = 3,
                      _turnQuant_lines = 2}),
        ("D",
         TurnQuantity{_turnQuant_roleId = "D", _turnQuant_turns = 2,
                      _turnQuant_chars = 14, _turnQuant_words = 3,
                      _turnQuant_lines = 2})])]]

  (map (foldTuplesStage1 addQuant) $ mkMapTuples _turnQuant_roleId id turnSample2')
