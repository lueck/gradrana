module GraDrAna.Tei
  ( parseRegisterOfPersons
  , parsePerson
  , teiNs
  ) where

import Text.XML.HXT.Core
import qualified Data.Map as Map

import GraDrAna.TypeDefs
import GraDrAna.ArrowXml

-- FIXME: ask this from an ArrowReaderT.
teiNs :: String
teiNs = "http://www.tei-c.org/ns/1.0"


-- * Parse the register of persons

-- | An arrow for parsing the register of persons.
parseRegisterOfPersons :: ArrowXml a => a XmlTree Person
parseRegisterOfPersons =
  isElem >>> hasQNameCase (mkNsName "castlist" teiNs) //>
  parsePerson
  -- FIXME: Do we need to handle castGroup? The only thing, we would
  -- get from it is a shared role description, but nothing substantial
  -- for our questions.

-- | An arrow for parsing a single person for the register of persons.
parsePerson :: ArrowXml a => a XmlTree Person
parsePerson =
  isElem >>> hasQNameCase (mkNsName "castitem" teiNs) >>>
  deep parseRoleId &&&
  (deep parseRoleName `orElse` arr (const Nothing)) &&&
  --arr (const $ Just "Role") &&&
  (deep parseRoleDesc `orElse` arr (const Nothing)) &&&
  --arr (const $ Just "Desc") &&&
  (deep parseGender `orElse` arr (const Nothing)) >>>
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
readGender "_" = Just Other
readGender _ = Nothing
