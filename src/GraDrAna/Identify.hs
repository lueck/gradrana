-- | Functions for identifying a speaker as a person from the registry
-- of persons.

module GraDrAna.Identify
  ( identify
  , identifyTurns
  , identifyTurnSpeakerAddIO
  , identifySpeakersAddIO
  ) where

import qualified Data.Map as Map
import Data.Char
import Data.List
import Control.Monad
import Data.Maybe
import Data.Default.Class
import Control.Lens

import GraDrAna.TypeDefs


-- | The main function for identifying a speaker as a person from the
-- registry of persons.
identify :: Persons -> String -> Maybe PersonId
identify reg who
  | Map.member whoId reg = Just whoId
  | Map.member who reg = Just who -- '#' forgotten
  -- FIXME: try to match a word from the role name
  | otherwise = Nothing
  where
    whoId = trim $ tail $ (++ " ") $ trim who -- remove '#' from the front
    trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Identify all the speakers in a list of 'Scene'.    
identifyTurns :: Persons -> [Scene] -> [Maybe PersonId]
identifyTurns reg scenes =
  concatMap ((map (join . (fmap (identify reg)) . _turn_roleId)) . _scene_turns) scenes'
  where
    scenes' = filter isSceneP scenes



-- | Like 'identify' but runs in the IO monad a prints a report. For a
-- helpful report, the 'Scene' is needed and the speaker is taken from
-- the 'Turn'.
identifyTurnSpeakerAddIO :: Persons -> Scene -> Turn -> IO (Persons)
identifyTurnSpeakerAddIO reg scene turn = do
  case join $ fmap (identify reg) (_turn_roleId turn) of
    Just _ -> return reg
    Nothing -> do
      putStrLn $ "Could not identify speaker '" ++
        who ++
        "' or '" ++
        (fromMaybe "[unkown]" role) ++
        "' in scene " ++
        (fromMaybe "[unkown]" $ fmap formatSceneNumber $ _scene_number scene) ++
        ".\n\tAdding."
      return $ Map.insert who (def
                                & person_id .~ who
                                & person_role .~ role) reg
  where
    role = _turn_role turn
    who = fromMaybe ("[unkown]" :: PersonId) $ fmap stripCross $ _turn_roleId turn
    stripCross = stripCross' . trim -- remove '#' from the front
    stripCross' ('#':r) = r
    stripCross' r = r
    trim = dropWhileEnd isSpace . dropWhile isSpace


-- | Try to identify the speakers in the play with regard to the
-- registry of persons. This runs in the IO monad and prints reports.
identifySpeakersAddIO :: Persons -> [Scene] -> IO (Persons)
identifySpeakersAddIO reg scenes = do
  let ids = identifyTurns reg scenes'
      noId = filter isNothing ids
  putStrLn $ (show $ length noId) ++ "/" ++ (show $ length ids) ++ " speakers could not be identified.\nReporting only some of them:"
  newReg <- identifySpeakersAddIO' reg scenes'
  -- report after adding
  let ids' = identifyTurns newReg scenes'
      noId' = filter isNothing ids'
  putStrLn $ "After updating the registry " ++
    (show $ length noId') ++ "/" ++ (show $ length ids) ++
    " speakers could not be identified."
  return newReg
  where
    scenes' = filter isSceneP scenes

identifySpeakersAddIO' :: Persons -> [Scene] -> IO (Persons)
identifySpeakersAddIO' reg [] = return reg
identifySpeakersAddIO' reg (s:scenes) = do
  ps <- mapM (identifyTurnSpeakerAddIO reg s) (_scene_turns s)
  let p = Map.union reg $ Map.unions ps
  identifySpeakersAddIO' p scenes
