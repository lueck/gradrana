{-# LANGUAGE TemplateHaskell #-}
module GraDrAna.TypeDefs where

import qualified Data.Map as Map
import Control.Lens
import Data.Default.Class
import Data.Maybe
import Data.List

-- * Persons or rather roles

-- | ADT representing genders.
data Gender = Female | Male | Intersexual | Other
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

makeLenses ''Person

-- | Default values of a 'Person' record.
instance Default Person where
  def = Person
        { _person_id = ""
        , _person_role = Nothing
        , _person_desc = Nothing
        , _person_gender = Nothing
        }

-- | A map representing the register of persons of a play.
type Persons = Map.Map PersonId Person

-- | Formatted string from 'Persons' in the play's registry.
formatPersons :: Persons -> String
formatPersons persons =
  "Found " ++ (show $ Map.size persons) ++ " persons in the registry:" ++
  concatMap (('\n':) . formatPerson . snd) (Map.toList persons) ++ "\n"

-- | Formatted string for a 'Person'.
formatPerson :: Person -> String
formatPerson p =
  (_person_id p) ++ " " ++
  (fromMaybe "[Name unknow]" $ fmap (map nice) $ _person_role p) ++ " " ++
  (fromMaybe "[No description]" $ fmap (map nice) $ _person_desc p) ++ " " ++
  (fromMaybe "[Gender unknown]" $ fmap show $ _person_gender p)
  where
    nice :: Char -> Char
    nice '\n' = ' '
    nice c = c


-- * Scenes

type SceneId = Int

-- | Number of a scene represented as a list of integers to express
-- nesting. This is 0-indexed, not 1-indexed as usual in dramatic
-- texts. So it's e.g. [2, 4] for third act, 5th scene.
type SceneNumber = [Int]

-- | A scene with speakers, ie. a part of a drama.
data Scene = Scene
  { _scene_id :: SceneId       -- ^ the scene's identifier (internal)
  , _scene_level :: Maybe Int  -- ^ level, e.g. 1 for acts, 2 for scenes
  , _scene_number :: Maybe SceneNumber      -- ^ the scene's number
  , _scene_head :: Maybe String             -- ^ the scene's heading
  , _scene_speakers :: Map.Map PersonId Int -- ^ map of active speakers
  , _scene_turns :: [Turn]                  -- ^ a list of turns
  } deriving (Show)

-- | Format the scene number to a 1-indexed representation
-- interspersed with dots.
formatSceneNumber :: SceneNumber -> String
formatSceneNumber = concat . (intersperse ".") . map (show . (+1))

-- | A map representing the scenes of a drama.
type Scenes = Map.Map SceneId Scene

-- | A Turn taken by a speaker.
data Turn = Turn
  { _turn_speaker :: Maybe PersonId -- ^ the speaker
  , _turn_turn :: Maybe String      -- ^ the spoken words
  , _turn_stages :: [String]          -- ^ stage directions during the turn
  } deriving (Show)

makeLenses ''Turn

-- | Default values of a turn.
instance Default Turn where
  def = Turn
        { _turn_speaker = Nothing
        , _turn_turn = Nothing
        , _turn_stages = []
        }

-- * Parser state

-- | A record for storing the state of the scene parser.
data SceneParserState = SceneParserState
  { _parser_sceneId :: Int
  , _parser_sceneNumber :: Maybe SceneNumber
  }

makeLenses ''SceneParserState

-- | Default state for scene parser.
instance Default SceneParserState where
  def = SceneParserState
        { _parser_sceneId = -1 -- 0-indexed
        , _parser_sceneNumber = Just []
        }
