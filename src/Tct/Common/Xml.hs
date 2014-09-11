-- | This module provides functions for generating XML content.
-- Currently not used.
module Tct.Common.Xml
  (
  )where

--import qualified Data.ByteString.Lazy as BS
--import qualified Data.Text as Txt
--import qualified Text.XML.Generator as Xml
--import Text.XML.Generator ((<#>))


--type XmlContent = Xml.Xml Xml.Elem
--type XmlAttribute = Xml.Xml Xml.Attr
--type XmlDocument = Xml.Xml Xml.Doc

--class Xml a where
  --toXml :: a -> XmlContent

--elt :: String -> [XmlAttribute] -> [XmlContent] -> XmlContent
--elt name attr children = Xml.xelem (Txt.pack name) $ Xml.xattrs attr <#> children

--strAttrib :: String -> String -> XmlAttribute
--strAttrib n s = Xml.xattr (Txt.pack n) (Txt.pack s)

--int :: (Integral i) => i -> XmlContent
--int i = Xml.xtext $ Txt.pack $ show $ toInteger i

--text :: String -> XmlContent
--text = Xml.xtext  . Txt.pack

--putXml :: Xml.Renderable t => Xml.Xml t -> IO ()
--putXml = BS.putStr . Xml.xrender

--toDocument :: XmlContent -> XmlDocument
--toDocument = Xml.doc Xml.defaultDocInfo

