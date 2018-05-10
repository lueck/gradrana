-- | Split a play on the basis of it's partition into scenes.

-- | Constructing a graph on the basis of whether a pair of characters
-- is present in the same scene is too simplistic. The approach in
-- 'GraDrAna.Splitter.TimeSlice' is more accurate.

module GraDrAna.Splitter.Scene where

import GraDrAna.TypeDefs

-- | Split the play's turns by the containing scene.
splitByScene :: [Scene] -> [[Turn]]
splitByScene = map _scene_turns . filter isSceneP

splitBySceneIO :: Persons -> [Scene] -> IO (Persons, [[Turn]])
splitBySceneIO reg scenes = return (reg, splitByScene scenes)
