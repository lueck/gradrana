{-# LANGUAGE FlexibleContexts #-}
-- | Split a play on the basis of it's partition into scenes.

-- | Constructing a graph on the basis of whether a pair of characters
-- is present in the same scene is too simplistic. The approach in
-- 'GraDrAna.Splitter.TimeSlice' is more accurate.

module GraDrAna.Splitter.Scene where

import GraDrAna.App
import GraDrAna.TypeDefs

-- | Split the play's turns by the containing scene.
splitByScenePure :: [Scene] -> [[Turn]]
splitByScenePure = map _scene_turns . filter isSceneP

-- | Same as 'splitByScenePure', but runs in a monad.
splitByScene :: AppConfig m => Persons -> [Scene] -> m (Persons, [[Turn]])
splitByScene reg scenes = return (reg, splitByScenePure scenes)
