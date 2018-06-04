{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Construct a graph on the basis of the quantity of turns (count of
-- turns, count of signifiers (words), count of characters, count of
-- lines). This results in a directed graph, where the edges' labels
-- represent the quantity of _utterance_ of a person being co-present
-- with an other.

-- | Combined with the simplistic scene splitter in
-- 'GraDrAna.Splitter.Scene' this implements the approach of the dlina
-- project by P. Trilcke et al.

module GraDrAna.Graph.TurnQuantity where

import qualified Data.Map as Map
import Data.Maybe
import Control.Lens
import Data.Default.Class
import Control.Applicative
import Control.Monad.Reader

import GraDrAna.App
import GraDrAna.TypeDefs
import GraDrAna.Graph.Common
import GraDrAna.Graph.GraphML


-- | Get the quantity of a turn. TODO: use tcf
quantity :: Turn -> EdgeLabel
quantity t = def
  & edgelabel_turns .~ (Just 1)
  & edgelabel_chars .~ (fmap length $ t^.turn_turn)
  & edgelabel_words .~ (fmap (length . words) $ t^.turn_turn)
  & edgelabel_lines .~ (fmap (length . lines) $ t^.turn_turn)

-- | Get all the turn quantities of the list of list of turns.
quantities :: [[Turn]] -> [[EdgeLabel]]
quantities = map (map quantity)

-- | Add two quantities.
addQuant :: EdgeLabel -> EdgeLabel -> EdgeLabel
addQuant a b = a
  & edgelabel_turns %~ (liftA2 (+) (_edgelabel_turns b))
  & edgelabel_chars %~ (liftA2 (+) (_edgelabel_chars b))
  & edgelabel_words %~ (liftA2 (+) (_edgelabel_words b))
  & edgelabel_lines %~ (liftA2 (+) (_edgelabel_lines b))

turnQuantity :: AppConfig m => Persons -> [[Turn]] -> m (Persons, [[Turn]])
turnQuantity reg turns =
  return $
  ( toRegistry mkLabel reg $
    foldTuples addQuant addQuant $
    mkMapTuples getRoleId quantity turns
  , turns)
  where
    getRoleId r = fromMaybe "UNKNOWN" (r^.turn_roleId)
    mkLabel = id


-- * Output to GraphML

-- | Generate a GraphML representation of a play.
turnQuantityGraphmlWriter :: Persons -> [[Turn]] -> App [Int]
turnQuantityGraphmlWriter reg _ = do
  getEdgeWeight <- asks _cfg_turnQuantity_getEdgeWeight
  rc <- runGraphmlWriter
        (mkGraphmlGraph
         rmLoops
         getEdgeWeight
         "directed"
         reg)
  return rc


-- * Exporting to dlina's "Zwischenformat"

-- FIXME: This is broken!

-- | Type for exporting to the XML "Zwischenformat" of the dlina project
--type Dlina = [[Maybe EdgeLabel]]
type Dlina = [[[(Maybe PersonId, EdgeLabel)]]]
--type Dlina = [Map.Map PersonId (Map.Map PersonId EdgeLabel)]
--type Dlina = [Mappy PersonId EdgeLabel]

-- | Return a list of pairs of 'PersonId' and 'EdgeLabel' for each
-- segment of a play, e.g. for each time slice. This can be used to
-- generate the "Zwischenformat" of the dlina project.
dlinaPure :: [[Turn]] -> Dlina
dlinaPure turns =
  map (getDlinas . (foldTuplesStage1 addQuant)) $
  mkMapTuples (^.turn_roleId) quantity turns
  where
    getDlinas m = map (uncurry getDlina) $ Map.toList m
    getDlina _ mp = Map.toList mp -- fmap snd $ listToMaybe $ Map.toList mp

-- | Like 'dlinaPure' but runs in the 'App' monad transformer stack.
dlina :: Persons -> [[Turn]] -> App (Persons, Dlina)
dlina reg turns = return (reg , (dlinaPure turns))
