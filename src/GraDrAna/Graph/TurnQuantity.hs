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

import GraDrAna.TypeDefs
import GraDrAna.Graph.Common


-- | A record for representing the quantity of a turn (or of multiple
-- turns).
data TurnQuantity
  = TurnQuantity
  { _turnQuant_roleId :: PersonId
  , _turnQuant_turns :: Int -- ^ dlina calls it speech_acts, which is not it
  , _turnQuant_chars :: Int -- ^ amount of characters
  , _turnQuant_words :: Int -- ^ amount of words
  , _turnQuant_lines :: Int -- ^ amount of lines
  }
  deriving (Show)

makeLenses ''TurnQuantity

-- | Default values of a 'TurnQuantity'. Since we add when collecting
-- turns, we set the default values to the neutral element of
-- addition.
instance Default TurnQuantity where
  def = TurnQuantity
        { _turnQuant_roleId = "UNKNOWN"
        , _turnQuant_turns = 0
        , _turnQuant_chars = 0
        , _turnQuant_words = 0
        , _turnQuant_lines = 0
        }

-- | Get the quantity of a turn. TODO: use tcf
quantity :: Turn -> TurnQuantity
quantity t = def
  & turnQuant_roleId .~ ((fromMaybe "UNKOWN") $ t^.turn_roleId)
  & turnQuant_turns .~ 1
  & turnQuant_chars .~ (fromMaybe 0 $ fmap length $ t^.turn_turn)
  & turnQuant_words .~ (fromMaybe 0 $ fmap (length . words) $ t^.turn_turn)
  & turnQuant_lines .~ (fromMaybe 0 $ fmap (length . lines) $ t^.turn_turn)

-- | Get all the turn quantities of the list of list of turns.
quantities :: [[Turn]] -> [[TurnQuantity]]
quantities = map (map quantity)

-- | Add two quantities.
addQuant :: TurnQuantity -> TurnQuantity -> TurnQuantity
addQuant a b = a
  & turnQuant_turns %~ (+ (_turnQuant_turns b))
  & turnQuant_chars %~ (+ (_turnQuant_chars b))
  & turnQuant_words %~ (+ (_turnQuant_words b))
  & turnQuant_lines %~ (+ (_turnQuant_lines b))


-- * Exporting to dlina's "Zwischenformat"

-- | Type for exporting to the XML "Zwischenformat" of the dlina project
type Dlina = [[Maybe TurnQuantity]]
--type Dlina = [[(PersonId, Maybe TurnQuantity)]]
--type Dlina = [Map.Map PersonId (Map.Map PersonId TurnQuantity)]
--type Dlina = [Mappy PersonId TurnQuantity]

-- | Return a list of pairs of 'PersonId' and 'TurnQuantity' for each
-- segment of a play, e.g. for each time slice. This can be used to
-- generate the "Zwischenformat" of the dlina project.
dlina :: [[Turn]] -> Dlina
dlina turns =
  map (getDlinas . (foldTuplesStage1 addQuant)) $
  mkMapTuples _turnQuant_roleId id $
  quantities turns
  where
    getDlinas m = map (uncurry getDlina) $ Map.toList m
    getDlina _ mp = fmap snd $ listToMaybe $ Map.toList mp

-- | Like 'dlina' but runs in the IO monad.
dlinaIO :: Persons -> [[Turn]] -> IO (Persons, Dlina)
dlinaIO reg turns = return (reg , (dlina turns))
