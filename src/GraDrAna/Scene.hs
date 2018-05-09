-- | Construct a graph on the basis of whether a pair of characters is
-- present in the same scene.
-- | This approach is too simplistic. 'GraDrAna.TimeSlice' is more
-- accurate.

module GraDrAna.Scene where

import GraDrAna.TypeDefs

-- | Split the play's turns by the containing scene.
splitByScene :: [Scene] -> [[Turn]]
splitByScene = map _scene_turns . filter isSceneP
