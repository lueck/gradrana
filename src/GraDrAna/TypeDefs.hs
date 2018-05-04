module GraDrAna.TypeDefs where

import qualified Data.Map as Map

-- | ADT representing possible genders.
data Gender = Female | Male | Inter | Other
  -- FIXME: Add some more!
  deriving (Show)

type PersonId = String

-- | A person or rather role.
data Person = Person
  { _person_id :: PersonId          -- ^ the role's identifier
  , _person_role :: Maybe String    -- ^ the name
  , _person_desc :: Maybe String    -- ^ the role's description
  , _person_gender :: Maybe Gender  -- ^ the role's gender
  } deriving (Show)

-- | A map representing the register of persons of a play.
type Persons = Map.Map PersonId Person

type SceneId = Int

type SceneCount = String

-- | A scene with speakers, ie. a part of a drama.
data Scene = Scene
  { _scene_id :: SceneId
  , _scene_level :: Maybe String
  , _scene_count :: Maybe SceneCount
  , _scene_head :: Maybe String
  , _scene_speakers :: Map.Map PersonId Int
  } deriving (Show)

-- | A map representing the scenes of a drama.
type Scenes = Map.Map SceneId Scene
