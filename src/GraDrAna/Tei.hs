module GraDrAna.Tei
  ( parseRegisterOfPersons
  , parsePerson
  , parseScene
  , teiNs
  ) where

import Text.XML.HXT.Core
import qualified Data.Map as Map
import Control.Lens hiding (deep)
import Text.Read

import GraDrAna.TypeDefs
import GraDrAna.ArrowXml

-- FIXME: ask this from an ArrowReaderT.
teiNs :: String
teiNs = "http://www.tei-c.org/ns/1.0"

-- * Parse the register of persons

-- | An arrow for parsing the register of persons.
parseRegisterOfPersons :: ArrowXml a => a XmlTree Persons
parseRegisterOfPersons =
  isElem >>> hasQNameCase (mkNsName "castlist" teiNs) >>>
  listA (multi parsePerson) >>>
  arr (Map.fromList . map (\p -> (_person_id p, p)))
  -- FIXME: Do we need to handle castGroup? The only thing, we would
  -- get from it is a shared role description, but nothing substantial
  -- for our questions.

-- | An arrow for parsing a single person for the register of persons.
parsePerson :: ArrowXml a => a XmlTree Person
parsePerson =
  isElem >>> hasQNameCase (mkNsName "castitem" teiNs) >>>
  deep parseRoleId &&&
  (deep parseRoleName `orElse` arrNothing) &&&
  --arr (const $ Just "Role") &&&
  (deep parseRoleDesc `orElse` arrNothing) &&&
  --arr (const $ Just "Desc") &&&
  (deep parseGender `orElse` arrNothing) >>>
  arr4 Person

-- | An arrow for parsing the role's identifier.
parseRoleId :: ArrowXml a => a XmlTree PersonId
parseRoleId =
  isElem >>> hasQNameCase (mkNsName "role" teiNs) >>>
  getAttrCaseValue "id"

-- | An arrow for parsing the role's name.
parseRoleName :: ArrowXml a => a XmlTree (Maybe String)
parseRoleName =
  -- FIXME: filter "rendition" element in order to get rid of prose
  -- behind a name?
  isElem >>> hasQNameCase (mkNsName "role" teiNs) >>>
  getAllText >>> arr Just

-- | An arrow for parsing the role description.
parseRoleDesc :: ArrowXml a => a XmlTree (Maybe String)
parseRoleDesc =
  isElem >>> hasQNameCase (mkNsName "roledesc" teiNs) >>>
  getAllText >>> arr Just

-- | An arrow for parsing the role's gender. If we can't parse this,
-- we need to make an inference using a database of first names.
parseGender :: ArrowXml a => a XmlTree (Maybe Gender)
parseGender =
  isElem >>> hasQNameCase (mkNsName "role" teiNs) >>>
  getAttrCaseValue "gender" >>> arr readGender

readGender :: String -> Maybe Gender
readGender "m" = Just Male
readGender "f" = Just Female
readGender "i" = Just Inter
readGender "_" = Just Other
readGender _ = Nothing


-- * Parsing who is present on stage and says something.

-- | Parse the roles that take a turn in a scene.  Important Note:
-- Higher level scenes have all the speaker turns from lower levels.
parseScene :: IOSLA (XIOState SceneParserState) XmlTree Scene
parseScene =
  isElem >>> hasQNameCase (mkNsName "div" teiNs) >>>
  changeUserState incSceneId >>>
  (getUserState >>> arr _parser_sceneId) &&&
  (parseSceneLevel `orElse` arrNothing) &&&
  (parseSceneCount `orElse` arrNothing) &&&
  (parseSceneHead `orElse` arrNothing) &&&
  parseSpeakers >>>
  arr5 Scene
  where
    incSceneId = (\b s -> s & parser_sceneId %~ (+1))

-- | Parse the level of a scene.
parseSceneLevel :: ArrowXml a => a XmlTree (Maybe Int)
parseSceneLevel =
  isElem >>> hasQNameCase (mkNsName "div" teiNs) >>>
  getAttrCaseValue "n" >>> arr readMaybe

-- | Make a scene count from level data and state.
parseSceneCount :: IOSLA (XIOState SceneParserState) XmlTree (Maybe SceneCount)
parseSceneCount =
  (parseSceneLevel `orElse` arrNothing) >>>
  changeUserState updateSceneCount >>>
  getUserState >>>
  arr _parser_sceneCount
  where
    updateSceneCount level s = s & parser_sceneCount %~ (incSceneCount level)
    incSceneCount level oldCnt = fmap incLast $ adjustLevel <$> level <*> oldCnt
    adjustLevel level oldCnt
      | level > length oldCnt = oldCnt++[-1] -- 0-indexed
      | level < length oldCnt = take level oldCnt
      | otherwise = oldCnt
    incLast cnt = init cnt ++ [(+1) $ last cnt] -- FIXME?

-- | Parse the head line of a scene.
parseSceneHead :: ArrowXml a => a XmlTree (Maybe String)
parseSceneHead =
  getChildren >>>
  isElem >>> hasQNameCase (mkNsName "head" teiNs) >>>
  getAllText >>> arr Just

-- | Parse the speakers of a scene. The integer value of a map entry
-- indicates how often they take their turn.
parseSpeakers :: ArrowXml a => a XmlTree (Map.Map PersonId Int)
parseSpeakers =
  listA (multi parseSpeaker) >>>
  arr (Map.fromListWith (+))

parseSpeaker :: ArrowXml a => a XmlTree (PersonId, Int)
parseSpeaker =
  isElem >>> hasQNameCase (mkNsName "sp" teiNs) >>>
  getAttrCaseValue "who" &&&
  arr (const 1) >>>
  arr2 (,)
