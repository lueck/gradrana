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

-- | A record for different variants of edge labels (weights).
data EdgeLabel = EdgeLabel
  { _edgelabel_copresence :: Maybe Int -- ^ count of occurance in the same time slice
  , _edgelabel_turns :: Maybe Int      -- ^ From turns to lines the fields are used
  , _edgelabel_chars :: Maybe Int      -- ^ to create a directed graph where the edge's
  , _edgelabel_words :: Maybe Int      -- ^ labels represent the amount of utterances.
  , _edgelabel_lines :: Maybe Int
  } deriving (Show)

makeLenses ''EdgeLabel

instance Default EdgeLabel where
  def = EdgeLabel Nothing Nothing Nothing Nothing Nothing

-- | A person or rather role.
data Person = Person
  { _person_id :: PersonId          -- ^ the role's identifier
  , _person_role :: Maybe String    -- ^ the name
  , _person_desc :: Maybe String    -- ^ the role's description
  , _person_gender :: Maybe Gender  -- ^ the role's gender
  , _person_edgesTo :: Map.Map PersonId EdgeLabel -- ^ edges to other persons
  } deriving (Show)

makeLenses ''Person

-- | Default values of a 'Person' record.
instance Default Person where
  def = Person
        { _person_id = ""
        , _person_role = Nothing
        , _person_desc = Nothing
        , _person_gender = Nothing
        , _person_edgesTo = Map.empty
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


-- * Turns

-- | A Turn taken by a speaker.
data Turn = Turn
  { _turn_roleId :: Maybe PersonId     -- ^ the speaker's Id
  , _turn_role :: Maybe String      -- ^ the speaker's role name
  , _turn_turn :: Maybe String      -- ^ the spoken words
  , _turn_stages :: [String]        -- ^ stage directions during the turn
  } deriving (Show)

makeLenses ''Turn

-- | Default values of a turn.
instance Default Turn where
  def = Turn
        { _turn_roleId = Nothing
        , _turn_role = Nothing
        , _turn_turn = Nothing
        , _turn_stages = []
        }

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
  }
  | Act
  { _act_id :: SceneId       -- ^ the scene's identifier (internal)
  , _act_level :: Maybe Int  -- ^ level, e.g. 1 for acts, 2 for scenes
  , _act_number :: Maybe SceneNumber      -- ^ the scene's number
  , _act_head :: Maybe String             -- ^ the scene's heading
  } deriving (Show)

makeLenses ''Scene

defaultScene :: Scene
defaultScene = Scene
  { _scene_id = 0
  , _scene_level = Nothing
  , _scene_number = Nothing
  , _scene_head = Nothing
  , _scene_speakers = Map.empty
  , _scene_turns = []
  }

defaultAct :: Scene
defaultAct = Act
  { _act_id = 0
  , _act_level = Nothing
  , _act_number = Nothing
  , _act_head = Nothing
  }

instance Default Scene where
  def = defaultScene

isSceneP :: Scene -> Bool
isSceneP (Scene _ _ _ _ _ _) = True
isSceneP _ = False

-- | Format the scene number to a 1-indexed representation
-- interspersed with dots.
formatSceneNumber :: SceneNumber -> String
formatSceneNumber = concat . (intersperse ".") . map (show . (+1))

-- | A map representing the scenes of a drama.
type Scenes = Map.Map SceneId Scene

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
