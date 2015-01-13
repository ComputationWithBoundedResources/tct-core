-- | This module provides functions for generating XML content.
module Tct.Core.Common.Xml
  (
  Xml (..)
  , XmlContent
  , XmlAttribute
  , XmlDocument
  , elt
  , elts
  , elts'
  , strAttrib
  , int
  , text
  , putXml
  , toDocument
  )where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text            as Txt
import           Text.XML.Generator   ((<#>))
import qualified Text.XML.Generator   as Xml


type XmlContent   = Xml.Xml Xml.Elem
type XmlAttribute = Xml.Xml Xml.Attr
type XmlDocument  = Xml.Xml Xml.Doc

class Xml a where
  toXml :: a -> XmlContent

instance Xml () where
  toXml _ = Xml.xempty

elt :: String -> XmlContent -> XmlContent
elt name child = Xml.xelem (Txt.pack name) $ Xml.xattrs [] <#> [child]

elts :: String -> [XmlContent] -> XmlContent
elts name children = Xml.xelem (Txt.pack name) $ Xml.xattrs [] <#> children

elts' :: String -> [XmlAttribute] -> [XmlContent] -> Xml.Xml Xml.Elem
elts' name atts children = Xml.xelem (Txt.pack name) $ Xml.xattrs atts <#> children

strAttrib :: String -> String -> XmlAttribute
strAttrib n s = Xml.xattr (Txt.pack n) (Txt.pack s)

int :: (Integral i) => i -> XmlContent
int i = Xml.xtext $ Txt.pack $ show $ toInteger i

text :: String -> XmlContent
text = Xml.xtext  . Txt.pack

putXml :: Xml.Renderable t => Xml.Xml t -> IO ()
putXml = BS.putStr . Xml.xrender

toDocument :: Maybe String -> XmlContent -> XmlDocument
toDocument s = Xml.doc $ Xml.defaultDocInfo{ Xml.docInfo_docType = s }

