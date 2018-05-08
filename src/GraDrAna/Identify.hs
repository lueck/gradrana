-- | Functions for identifying a speaker as a person from the registry
-- of persons.

module GraDrAna.Identify
  ( identify
  , identifyTurns
  , identifyTurnSpeakerIO
  , identifySpeakersIO
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
  concatMap ((map (join . (fmap (identify reg)) . _turn_speaker)) . _scene_turns) scenes

-- | Like 'identify' but runs in the IO monad a prints a report. For a
-- helpful report, the 'Scene' is needed and the speaker is taken from
-- the 'Turn'.
identifyTurnSpeakerIO :: Persons -> Scene -> Turn -> IO (Persons)
identifyTurnSpeakerIO reg scene turn = do
  case join $ fmap (identify reg) speaker of
    Just _ -> return reg
    Nothing -> do
      putStrLn $ "Could not identify speaker '" ++
        fromMaybe "" speaker ++
        "' in scene " ++
        (fromMaybe "[unkown]" $ fmap formatSceneNumber $ _scene_number scene) ++
        ".\n Adding."
      return $ Map.insertWith
        takeOld
        (fromMaybe "[unkown]" speaker)
        (def & person_role .~ speaker)
        reg
  where
    speaker = _turn_speaker turn
    takeOld a _ = a

-- | Try to identify the speakers in the play with regard to the
-- registry of persons. This runs in the IO monad and prints reports.
identifySpeakersIO :: Persons -> [Scene] -> IO (Persons)
identifySpeakersIO reg scenes = do
  let ids = identifyTurns reg scenes
      noId = filter isNothing ids
  putStrLn $ (show $ length noId) ++ "/" ++ (show $ length ids) ++ " speakers could not be identified."
  newReg <- identifySpeakersIO' reg scenes
  -- report after adding
  let ids' = identifyTurns newReg scenes
      noId' = filter isNothing ids'
  putStrLn $ (show $ length noId') ++ "/" ++ (show $ length ids) ++ " speakers could not be identified."
  return newReg

identifySpeakersIO' :: Persons -> [Scene] -> IO (Persons)
identifySpeakersIO' reg [] = return reg
identifySpeakersIO' reg (s:scenes) = do
  ps <- mapM (identifyTurnSpeakerIO reg s) (_scene_turns s)
  let p = Map.unions ps
  identifySpeakersIO' p scenes
