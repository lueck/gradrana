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

nameIn :: (ArrowXml a) => [String] -> a XmlTree XmlTree
nameIn names = (getName >>> isA (flip elem names)) `guards` this
{-# INLINE nameIn #-}

qNameIn :: (ArrowXml a) => [QName] -> a XmlTree XmlTree
qNameIn qNames = (getQName >>> isA (flip elem qNames)) `guards` this
{-# INLINE qNameIn #-}

stripName :: (ArrowXml a) => String -> a XmlTree XmlTree
stripName n = processTopDown (filterA $ neg (hasName n))
{-# INLINE stripName #-}

-- this does not work, we need neg or ifA to pass nodes through
stripName' :: (ArrowXml a) => String -> a XmlTree XmlTree
stripName' n = processTopDown (filterA (getName >>> isA (/= n)) `guards` this)

stripNames :: (ArrowXml a) => [String] -> a XmlTree XmlTree
stripNames names = processTopDown $ neg $ (nameIn names) `guards` this
{-# INLINE stripNames #-}

stripQNames :: (ArrowXml a) => [QName] -> a XmlTree XmlTree
stripQNames qNames = processTopDown $ neg $ (qNameIn qNames) `guards` this

-- | Select the value of an attribute of an element node. The
-- attribute name is matched case insensitive. This always succeeds
-- with an empty string as default value.
getAttrCaseValue :: (ArrowXml a) => String -> a XmlTree String
getAttrCaseValue n =
  xshow (getAttrl >>>
         hasNameWith ((==n') . upper . localPart) >>>
         getChildren)
  where
    n' = upper n
    upper = map toUpper
{-# INLINE getAttrCaseValue #-}

-- | Like 'getAttrCaseValue', but fails if the attribute does not exist
getAttrCaseValue0 :: (ArrowXml a) => String -> a XmlTree String
getAttrCaseValue0 n =
  getAttrl >>>
  hasNameWith ((==n') . upper . localPart) >>>
  xshow getChildren
  where
    n' = upper n
    upper = map toUpper
{-# INLINE getAttrCaseValue0 #-}

caseFun :: Char -> Char
caseFun = toUpper
{-# INLINE caseFun #-}

makeQNameCase :: QName -> QName
makeQNameCase n = mkQName (map caseFun $ namePrefix n) (map caseFun $ localPart n) (map caseFun $ namespaceUri n)
{-# INLINE makeQNameCase #-}

hasQNameCase :: (ArrowXml a) => QName -> a XmlTree XmlTree
hasQNameCase n = (getQNameCase >>> isA (== (makeQNameCase n))) `guards` this
{-# INLINE hasQNameCase #-}

getQNameCase :: (ArrowXml a) => a XmlTree QName
getQNameCase = arrL (maybeToList . (fmap makeQNameCase) . XN.getName)
{-# INLINE getQNameCase #-}

qNameCaseIn :: (ArrowXml a) => [QName] -> a XmlTree XmlTree
qNameCaseIn qNames = -- (getQNameCase >>> isA (flip elem qNames)) `guards` this
  qNameIn qNames -- FIXME: above code does not work
{-# INLINE qNameCaseIn #-}

stripQNamesCase :: (ArrowXml a) => [QName] -> a XmlTree XmlTree
stripQNamesCase qNames = processTopDown $ neg $ (qNameCaseIn qNames) `guards` this


getAllText :: (ArrowXml a) => a XmlTree String
getAllText = listA (multi (isText >>> getText)) >>> arr concat
{-# INLINE getAllText #-}

arrNothing :: (ArrowXml a) => a XmlTree (Maybe b)
arrNothing =  arr (const Nothing)


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

