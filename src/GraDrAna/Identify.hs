{-# LANGUAGE FlexibleContexts #-}
-- | Functions for identifying a speaker as a person from the registry
-- of persons.

module GraDrAna.Identify
  ( identify
  , identifyTurns
  , identifyTurnSpeaker
  , identifySpeakers
  , identifyTurnSpeakerAdd
  , identifySpeakersAdd
  , adjustRoleIdsPure
  , adjustRoleIds
  ) where

import qualified Data.Map as Map
import Data.Char
import Data.List
import Control.Monad
import Data.Maybe
import Data.Default.Class
import Control.Lens
import System.IO
import Control.Monad.Trans

import GraDrAna.TypeDefs
import GraDrAna.App
import GraDrAna.IO

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


-- | Like 'identify' but runs in the App monad a prints a report. For a
-- helpful report, the 'Scene' is needed and the speaker is taken from
-- the 'Turn'.
identifyTurnSpeaker :: Persons -> Scene -> Turn -> App ()
identifyTurnSpeaker reg scene turn = do
  case join $ fmap (identify reg) speaker of
    Just _ -> return ()
    Nothing -> do
      liftIO $ putStrLn $ "Could not identify speaker '" ++
        fromMaybe "" speaker ++
        "' or '" ++
        (fromMaybe "[unkown]" role) ++
        "' in scene " ++
        (fromMaybe "[unkown]" $ fmap formatSceneNumber $ _scene_number scene) ++
        "."
  where
    role = _turn_role turn
    speaker = _turn_roleId turn

-- | Try to identify the speakers in the play with regard to the
-- registry of persons. This runs in the App monad and prints reports.
identifySpeakers :: Persons -> [Scene] -> App ()
identifySpeakers reg scenes = do
  let ids = identifyTurns reg scenes
      noId = filter isNothing ids
  liftIO $ putStrLn $ (show $ length noId) ++ "/" ++ (show $ length ids) ++ " speakers could not be identified."
  identifySpeakers' reg scenes'
  where
    scenes' = filter isSceneP scenes

identifySpeakers' :: Persons -> [Scene] -> App ()
identifySpeakers' _ [] = return ()
identifySpeakers' reg (s:scenes) = do
  mapM (identifyTurnSpeaker reg s) (_scene_turns s)
  identifySpeakers' reg scenes


-- | Like 'identifySpeakers' but adds all unkown speakers to the
-- registry of persons.
identifySpeakersAdd :: Persons -> [Scene] -> App (Persons, [Scene])
identifySpeakersAdd reg scenes = do
  let ids = identifyTurns reg scenes'
      noId = filter isNothing ids
  logLevel 10 $ (show $ length noId) ++ "/" ++ (show $ length ids) ++ " speakers could not be identified.\nNon-exhaustive report where duplicates in subsequent scenes are left:"
  newReg <- identifySpeakersAdd' reg scenes'
  -- report after adding
  let ids' = identifyTurns newReg scenes'
      noId' = filter isNothing ids'
  logLevel 10 $ "After updating the registry " ++
    (show $ length noId') ++ "/" ++ (show $ length ids) ++
    " speakers could not be identified."
  return (newReg, scenes)
  where
    scenes' = filter isSceneP scenes

identifySpeakersAdd' :: Persons -> [Scene] -> App (Persons)
identifySpeakersAdd' reg [] = return reg
identifySpeakersAdd' reg (s:scenes) = do
  ps <- mapM (identifyTurnSpeakerAdd reg s) (_scene_turns s)
  let p = Map.union reg $ Map.unions ps
  identifySpeakersAdd' p scenes

-- | Like 'identifyTurnSpeaker' but adds an unkown speaker to the
-- registry of persons.
identifyTurnSpeakerAdd :: Persons -> Scene -> Turn -> App (Persons)
identifyTurnSpeakerAdd reg scene turn = do
  case join $ fmap (identify reg) (_turn_roleId turn) of
    Just _ -> return reg
    Nothing -> do
      logLevel 10 $ "Could not identify speaker '" ++
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


-- | Remove the leading character # from the role identifiers of the
-- turns in scenes.
adjustRoleIdsPure :: Persons -> [Scene] -> [Scene]
adjustRoleIdsPure reg scenes = map (scene_turns %~ (adjustTurns reg)) scenes
  where
    adjustTurns reg turns = map (adjustTurn reg) turns
    adjustTurn reg turn = turn & turn_roleId %~ (adjustRoleId reg)
    adjustRoleId reg oldId = join $ fmap (identify reg) oldId

-- | Like 'adjustRoleIdsPure' but runs in the App monad.
adjustRoleIds :: AppConfig m => Persons -> [Scene] -> m (Persons, [Scene])
adjustRoleIds reg scenes = return (reg, adjustRoleIdsPure reg scenes)
