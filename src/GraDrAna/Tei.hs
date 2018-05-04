module GraDrAna.Tei where

import Text.XML.HXT.Core
import qualified Data.Map as Map

import GraDrAna.TypeDefs
import GraDrAna.ArrowXml

teiNs :: String
teiNs = "http://www.tei-c.org/ns/1.0"

parseRegisterOfPersons :: ArrowXml a => a XmlTree Person
parseRegisterOfPersons =
  isElem >>> hasQNameCase (mkNsName "castlist" teiNs) //>
  parsePerson

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

parseRoleId :: ArrowXml a => a XmlTree PersonId
parseRoleId =
  isElem >>> hasQNameCase (mkNsName "role" teiNs) >>>
  getAttrCaseValue "id"

parseRoleName :: ArrowXml a => a XmlTree (Maybe String)
parseRoleName =
  -- FIXME: filter "rendition" element in order to get rid of prose
  -- behind a name?
  isElem >>> hasQNameCase (mkNsName "role" teiNs) >>>
  getAllText >>> arr Just
  
parseRoleDesc :: ArrowXml a => a XmlTree (Maybe String)
parseRoleDesc =
  isElem >>> hasQNameCase (mkNsName "roledesc" teiNs) >>>
  getAllText >>> arr Just

parseGender :: ArrowXml a => a XmlTree (Maybe Gender)
parseGender =
  isElem >>> hasQNameCase (mkNsName "role" teiNs) >>>
  getAttrCaseValue "gender" >>> arr readGender

readGender :: String -> Maybe Gender
readGender "m" = Just Male
readGender "f" = Just Female
readGender "_" = Just Other
readGender _ = Nothing
