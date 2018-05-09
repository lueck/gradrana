module GraDrAna.Tei
  ( parseRegisterOfPersons
  , parsePerson
  , parseDivision
  , parseAct
  , parseScene
  , teiNs
  , runTeiParsers
  ) where

-- | Parsing a TEI encoded play. Use 'runTeiParses' for getting the
-- register of persons and a list of scene records with speakers and
-- turn taking.

-- | We don't really parse XML here. With the hxt library
-- ('Text.XML.HXT') we do not need to touch XML. It is parsed by a
-- generic parser into a recursive data structure. We get the
-- interesting data from this 'XmlTree' using arrows. The generic
-- parser is run by 'readDocument' from within 'runTeiParsers'.

import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow (runXIOState, initialState)
import qualified Data.Map as Map
import Control.Lens hiding (deep)
import Text.Read
import Data.Maybe
import Data.Default.Class

import GraDrAna.TypeDefs
import GraDrAna.ArrowXml

-- FIXME: ask this from an ArrowReaderT.
teiNs :: String
teiNs = "http://www.tei-c.org/ns/1.0"

-- * Parse the register of persons

-- | An arrow for parsing the dramatis personae or register of
-- persons.
parseRegisterOfPersons :: ArrowXml a => a XmlTree Persons
parseRegisterOfPersons =
  isElem >>> hasQNameCase (mkNsName "castlist" teiNs) >>>
  listA (multi parsePerson) >>>
  arr (Map.fromList . map (\p -> (_person_id p, p)))
  -- FIXME: Do we need to handle castGroup? The only thing, we would
  -- get from it is a shared role description, but nothing substantial
  -- for our questions.--No! Wait. A shared description may and should
  -- be used to connect persons, not on the ground of their actions,
  -- but on the ground of play's setting.

-- | An arrow for parsing a single person for the register of persons.
parsePerson :: ArrowXml a => a XmlTree Person
parsePerson =
  isElem >>> hasQNameCase (mkNsName "castitem" teiNs) >>>
  deep parseRoleId &&&
  (deep parseRoleName `orElse` arrNothing) &&&
  (deep parseRoleDesc `orElse` arrNothing) &&&
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
  getAttrCaseValue0 "gender" >>> arr readGender

readGender :: String -> Maybe Gender
readGender "m" = Just Male
readGender "f" = Just Female
readGender "i" = Just Intersexual
readGender "_" = Just Other
readGender _ = Nothing


-- * Parsing who is present on stage and says something.

-- | Parse a division of a play. Devisions may be nested, but we
-- assume that a turns of a devision is the sum of the turns in its
-- subdevisions. So: Either the devision contains other devisions, or
-- it contains turns as direct childs.
parseDivision :: IOSLA (XIOState SceneParserState) XmlTree Scene
parseDivision =
  isElem >>> hasQNameCase (mkNsName "div" teiNs) >>>
  ifA (getChildren >>> deep (hasQNameCase (mkNsName "div" teiNs))) parseAct parseScene

-- | Parse an act, ie. a division, that contains other divisions but
-- no speakers' turns.
parseAct :: IOSLA (XIOState SceneParserState) XmlTree Scene
parseAct =
  isElem >>> hasQNameCase (mkNsName "div" teiNs) >>>
  changeUserState incSceneId >>>
  (getUserState >>> arr _parser_sceneId) &&&
  (parseSceneLevel `orElse` arrNothing) &&&
  (parseSceneNumber `orElse` arrNothing) &&&
  (parseSceneHead `orElse` arrNothing) >>>
  arr4 Act
  where
    incSceneId = (\b s -> s & parser_sceneId %~ (+1))

-- | Parse a scene with speakers' turns in it.
parseScene :: IOSLA (XIOState SceneParserState) XmlTree Scene
parseScene =
  isElem >>> hasQNameCase (mkNsName "div" teiNs) >>>
  changeUserState incSceneId >>>
  (getUserState >>> arr _parser_sceneId) &&&
  (parseSceneLevel `orElse` arrNothing) &&&
  (parseSceneNumber `orElse` arrNothing) &&&
  (parseSceneHead `orElse` arrNothing) &&&
  parseSpeakers &&&
  listA (multi parseTurnTaking) >>>
  arr6 Scene
  where
    incSceneId = (\b s -> s & parser_sceneId %~ (+1))

-- | Parse the level of a scene.
parseSceneLevel :: ArrowXml a => a XmlTree (Maybe Int)
parseSceneLevel =
  isElem >>> hasQNameCase (mkNsName "div" teiNs) >>>
  getAttrCaseValue "n" >>> arr readMaybe

-- | Make a scene number from level data and state.
parseSceneNumber :: IOSLA (XIOState SceneParserState) XmlTree (Maybe SceneNumber)
parseSceneNumber =
  (parseSceneLevel `orElse` arrNothing) >>>
  changeUserState updateSceneNumber >>>
  getUserState >>>
  arr _parser_sceneNumber
  where
    updateSceneNumber level s = s & parser_sceneNumber %~ (incSceneNumber level)
    incSceneNumber level oldNo = fmap incLast $ adjustLevel <$> (assertPos level) <*> oldNo
    adjustLevel level oldNo
      | level > length oldNo = oldNo++[-1] -- 0-indexed
      | level < length oldNo = take level oldNo
      | otherwise = oldNo
    incLast n = init n ++ [(+1) $ last n]
    -- Assert level > 0, otherwise the list processing would fail.
    assertPos Nothing = Nothing
    assertPos (Just l)
      | l > 0 = Just l
      | otherwise = Nothing

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
  listA (multi parseSpeak) >>>
  arr (Map.fromListWith (+))

-- | @Maybe PersonId@ would not be a valid key type, so the string
-- "UNKNOWN" is used when no speaker name was found.
parseSpeak :: ArrowXml a => a XmlTree (PersonId, Int)
parseSpeak =
  isElem >>> hasQNameCase (mkNsName "sp" teiNs) >>>
  parseWhoSpeaks >>> arr (fromMaybe "UNKNOWN") &&&
  arr (const 1) >>>
  arr2 (,)

-- | Parse who's speaking. First try to get the information from the
-- @who@ attribute of the @sp@ element. If it is not present, try to
-- get the text node from the @speaker@ element. If that fails to,
-- 'Nothing' is returned.
parseWhoSpeaks :: ArrowXml a => a XmlTree (Maybe PersonId)
parseWhoSpeaks =
  isElem >>> hasQNameCase (mkNsName "sp" teiNs) >>>
  (getAttrCaseValue0 "who" >>> arr Just)
  `orElse`
  (deep (isElem >>> hasQNameCase (mkNsName "speaker" teiNs)) >>>
    getAllText >>> arr Just)
  `orElse`
  arrNothing

-- | Parse the @who@ attribute of a @sp@ element or fail.
parseWho :: ArrowXml a => a XmlTree (Maybe PersonId)
parseWho =
  isElem >>> hasQNameCase (mkNsName "sp" teiNs) >>>
  getAttrCaseValue0 "who" >>> arr Just

-- | Parse the @speaker@ neested in the current subtree or fail.
parseSpeaker :: ArrowXml a => a XmlTree (Maybe String)
parseSpeaker =
  deep (isElem >>> hasQNameCase (mkNsName "speaker" teiNs)) >>>
  getAllText >>> arr Just

-- | Parse the taking of a turn by a speaker.
parseTurnTaking :: ArrowXml a => a XmlTree Turn
parseTurnTaking =
  isElem >>> hasQNameCase (mkNsName "sp" teiNs) >>>
  (parseWho `orElse` arrNothing) &&&
  (parseSpeaker `orElse` arrNothing) &&&
  parseTurn &&&
  listA (multi parseStage) >>>
  arr4 makeTurn
  where
    makeTurn who role turn stages = def
      & turn_roleId .~ who
      & turn_role .~ role
      & turn_turn .~ (Just turn)
      & turn_stages .~ stages

-- | Parse a turn, strip speaker name and stage directions.
parseTurn :: ArrowXml a => a XmlTree String
parseTurn =
  stripQNamesCase [ (mkNsName "speaker" teiNs)
                  , (mkNsName "stage" teiNs) ] >>>
  getAllText

-- | Parse a stage direction.
parseStage :: ArrowXml a => a XmlTree String
parseStage =
  isElem >>> hasQNameCase (mkNsName "stage" teiNs) >>>
  getAllText


-- * Running the parsers

-- | Run parser for the register of persons and the scenes on a TEI
-- file. Return a tuple of 'Persons' and a list of 'Scene'.
runTeiParsers :: String -- ^ file name of TEI file
              -> IO (Persons, [Scene])
runTeiParsers fName = do
  tree <- runX (readDocument [withValidate no] fName >>>
                propagateNamespaces)
  roles <- runX (constL tree //>
                 single parseRegisterOfPersons)
  scenes <- runXIOState
            (initialState def)
            (constL tree //>
             multi parseDivision)
  return (head roles, scenes)
