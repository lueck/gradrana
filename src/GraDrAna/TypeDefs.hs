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
