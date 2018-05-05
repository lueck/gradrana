module GraDrAna.ArrowXml
  ( nameIn
  , qNameIn
  , stripName
  , stripNames
  , stripQNames
  , getAttrCaseValue
  , getAttrCaseValue0
  , hasQNameCase
  , getQNameCase
  , makeQNameCase
  , qNameCaseIn
  , stripQNamesCase
  , getAllText
  , arrNothing
  , arr5
  , arr6
  ) where

import Text.XML.HXT.Core
import qualified Text.XML.HXT.DOM.XmlNode as XN
import Data.Char
import Data.Maybe

-- * Basic functions and arrows for case insensitiv matching

-- | Case convertion for case insensitive matches.
caseFun :: Char -> Char
caseFun = toUpper
{-# INLINE caseFun #-}

-- | Convert the case of a 'QName' using 'caseFun'.
makeQNameCase :: QName -> QName
makeQNameCase n = mkQName (map caseFun $ namePrefix n) (map caseFun $ localPart n) (map caseFun $ namespaceUri n)
{-# INLINE makeQNameCase #-}

-- | Case insensitive variant of hxt's 'hasQName'.
hasQNameCase :: (ArrowXml a) => QName -> a XmlTree XmlTree
hasQNameCase n = (getQNameCase >>> isA (== (makeQNameCase n))) `guards` this
{-# INLINE hasQNameCase #-}

-- | Case insensitive variant of hxt's 'getQName'.
getQNameCase :: (ArrowXml a) => a XmlTree QName
getQNameCase = arrL (maybeToList . (fmap makeQNameCase) . XN.getName)
{-# INLINE getQNameCase #-}

-- * Accessing attributes case insensitively

-- | Select the value of an attribute of an element node. The
-- attribute name is matched case insensitive. This always succeeds
-- with an empty string as default value. This is a case insensitive
-- variant of hxt's 'getAttrValue'.
getAttrCaseValue :: (ArrowXml a) => String -> a XmlTree String
getAttrCaseValue n =
  xshow (getAttrl >>>
         hasNameWith ((==n') . upper . localPart) >>>
         getChildren)
  where
    n' = upper n
    upper = map caseFun
{-# INLINE getAttrCaseValue #-}

-- | Like 'getAttrCaseValue', but fails if the attribute does not
-- exist. This is a case insensitive variant of hxt's 'getAttrValue0'.
getAttrCaseValue0 :: (ArrowXml a) => String -> a XmlTree String
getAttrCaseValue0 n =
  getAttrl >>>
  hasNameWith ((==n') . upper . localPart) >>>
  xshow getChildren
  where
    n' = upper n
    upper = map caseFun
{-# INLINE getAttrCaseValue0 #-}

-- * Matching names of elements or attributes against a list

-- | Matching against a list of unqualified names.
nameIn :: (ArrowXml a) => [String] -> a XmlTree XmlTree
nameIn names = (getName >>> isA (flip elem names)) `guards` this
{-# INLINE nameIn #-}

-- | Matching against a list of qualified names.
qNameIn :: (ArrowXml a) => [QName] -> a XmlTree XmlTree
qNameIn qNames = (getQName >>> isA (flip elem qNames)) `guards` this
{-# INLINE qNameIn #-}

-- | Case insensitive variant of 'qNameIn'.
qNameCaseIn :: (ArrowXml a) => [QName] -> a XmlTree XmlTree
qNameCaseIn qNames = (getQNameCase >>> isA (flip elem qNames')) `guards` this
  where
    qNames' = map makeQNameCase qNames
{-# INLINE qNameCaseIn #-}

-- * Stripping elements from the tree

-- | Strip elements from the tree if their name matches against a
-- unqualified name.
stripName :: (ArrowXml a) => String -> a XmlTree XmlTree
stripName n = processTopDown (filterA $ neg (hasName n))
{-# INLINE stripName #-}

-- this does not work, we need neg or ifA to pass nodes through
stripName' :: (ArrowXml a) => String -> a XmlTree XmlTree
stripName' n = processTopDown (filterA (getName >>> isA (/= n)) `guards` this)

-- | Strip elements from the tree if their name matches a member of a
-- list of unqualified names.
stripNames :: (ArrowXml a) => [String] -> a XmlTree XmlTree
stripNames names = processTopDown $ neg $ (nameIn names) `guards` this
{-# INLINE stripNames #-}

-- | Like 'stripNames' but for qualified names.
stripQNames :: (ArrowXml a) => [QName] -> a XmlTree XmlTree
stripQNames qNames = processTopDown $ neg $ (qNameIn qNames) `guards` this
{-# INLINE stripQNames #-}

-- | Case insensitive variant of 'stripQNames'.
stripQNamesCase :: (ArrowXml a) => [QName] -> a XmlTree XmlTree
stripQNamesCase qNames = processTopDown $ neg $ (qNameCaseIn qNames) `guards` this
{-# INLINE stripQNamesCase #-}

-- * Misc

-- | Get the text nodes from all children and return them as a single
-- concanated string.
getAllText :: (ArrowXml a) => a XmlTree String
getAllText = listA (multi (isText >>> getText)) >>> arr concat
{-# INLINE getAllText #-}

-- | An arrow that returns 'Nothing'.
arrNothing :: (ArrowXml a) => a XmlTree (Maybe b)
arrNothing =  arr (const Nothing)
{-# INLINE arrNothing #-}

-- | construction of a 5 argument arrow from a 5-ary function
-- |
-- | example: @ a1 &&& a2 &&& a3 &&& a4 &&& a5 >>> arr5 f @
arr5 :: Arrow a => (b1 -> b2 -> b3 -> b4 -> b5 -> c) -> a (b1, (b2, (b3, (b4, b5)))) c
arr5 f = arr (\ ~(x1, ~(x2, ~(x3, ~(x4, x5)))) -> f x1 x2 x3 x4 x5)
{-# INLINE arr5 #-}

-- | construction of a 6 argument arrow from a 6-ary function
arr6 :: Arrow a => (b1 -> b2 -> b3 -> b4 -> b5 -> b6 -> c) -> a (b1, (b2, (b3, (b4, (b5, b6))))) c
arr6 f = arr (\ ~(x1, ~(x2, ~(x3, ~(x4, (x5, x6))))) -> f x1 x2 x3 x4 x5 x6)
{-# INLINE arr6 #-}


play :: String -> IO ()
play fname = do
  results <- runX (readDocument [withValidate no] fname >>>
                   getChildren >>>
                   isElem >>> hasName "html" >>>
                   getChildren >>>
                   isElem >>> hasName "body" >>>
                   stripName "p" >>>
                   getChildren >>>
                   isElem >>>
                   getChildren >>>
                   getText)
  print results

