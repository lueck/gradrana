module GraDrAna.TypeDefs where

import qualified Data.Map as Map

type PersonId = String

data Gender = Male | Female | Other
  deriving (Show)

data Person = Person
  { _person_id :: PersonId
  , _person_role :: Maybe String
  , _person_desc :: Maybe String
  , _person_gender :: Maybe Gender
  } deriving (Show)

type Persons = Map.Map PersonId Person
