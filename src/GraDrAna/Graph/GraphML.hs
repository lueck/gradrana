module GraDrAna.Graph.GraphML where

import Text.XML.HXT.Core


graphmlNs :: String
graphmlNs = "http://graphml.graphdrawing.org/xmlns"

graphmlSchemaLoc :: String
graphmlSchemaLoc = "http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd"

xsiNs :: String
xsiNs = "http://www.w3.org/2001/XMLSchema-instance"


runGraphmlWriter ::
  FilePath                                 -- ^ the output file
  -> IOSLA (XIOState ()) XmlTree XmlTree -- ^ arrow for making the graph element
  -> IO [Int]
runGraphmlWriter fName body = do
  rc <- runX (mkGraphmlDoc body >>>
              writeDocument [withIndent yes] fName >>>
              getErrStatus)
  return rc

mkGraphmlDoc :: (ArrowXml a)
  => a XmlTree XmlTree        -- ^ arrow for making the graph element
  -> a XmlTree XmlTree          -- ^ returns an xml arrow
mkGraphmlDoc graphEl =
  root                          -- make the hxt root node containing XML-Decl etc.
  []                            -- its attribute nodes
  [                             -- child nodes
    (mkqelem -- xml root element
      (mkQName "" "graphml" graphmlNs)
      [ -- attributes
        (sattr "xmlns" graphmlNs)
      , (sattr "xmlns:xsi" xsiNs)
      , (sattr "xsi:schemaLocation" $ graphmlNs ++ " " ++ graphmlSchemaLoc)
      ]
      [ graphEl ] -- child nodes
    )
  ]
