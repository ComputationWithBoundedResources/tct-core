-- | This module provides functions for generating XML content.
module Tct.Core.Common.Xml
  (
  Xml (..)
  , XmlContent
  , XmlAttribute
  , XmlDocument
  , elt
  , int
  , text
  , att
  , setAtts
  -- * output
  , toDocument
  , putXml
  -- * search and manipulation
  , search
  , find
  , rootTag
  , child
  , children
  , addChildren
  ) where


import qualified Data.ByteString.Lazy  as BS
import           Data.Monoid
import           Data.Maybe (fromMaybe)
import qualified Data.Text             as Txt
import qualified Data.Text.IO          as Txt (putStr)
import qualified Text.XML.Expat.Format as Xml (formatNode)
import qualified Text.XML.Expat.Tree   as Xml
import qualified Text.XML.Expat.Proc   as Xml


type XmlContent   = Xml.UNode Txt.Text
type XmlAttribute = (Txt.Text, Txt.Text)
type XmlDocument  = (Txt.Text, XmlContent)

class Xml a where
  toXml :: a -> XmlContent

instance Xml () where
  toXml _ = text ""

elt :: String -> [XmlContent] -> XmlContent
elt name = Xml.Element (Txt.pack name) []

setAtts :: [XmlAttribute] -> XmlContent -> XmlContent
setAtts atts e = e{ Xml.eAttributes = atts }

att :: String -> String -> XmlAttribute
att n s = (Txt.pack n, Txt.pack s)

int :: (Integral i) => i -> XmlContent
int i = Xml.Text . Txt.pack . show $ toInteger i

text :: String -> XmlContent
text = Xml.Text . Txt.pack

toDocument :: Maybe String -> XmlContent -> XmlDocument
toDocument Nothing c  = (mempty, c)
toDocument (Just s) c = (Txt.pack $ header ++ s ++ "\n", c)
  where header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" 

putXml :: XmlDocument -> IO ()
putXml (header, content)= Txt.putStr header >> BS.putStr (Xml.formatNode content)

search :: String -> XmlContent -> Maybe XmlContent
search s = Xml.findElement (Txt.pack s)

find :: String -> XmlContent -> XmlContent
find s c = err `fromMaybe` search s c
  where err = error $ "Tct.Core.Common.Xml.find: element not found " ++ s ++ " ."

rootTag :: XmlContent -> String
rootTag (Xml.Element t _ _) = Txt.unpack t
rootTag _                   = ""

child :: XmlContent -> XmlContent
child (Xml.Element _ _ [e]) = e
child _                     = error "Tct.Core.Common.Xml.children: not a single child."

children :: XmlContent -> [XmlContent]
children (Xml.Element _ _ es) = es
children (Xml.Text _)         = []

-- | Adds elements below the root element.
addChildren :: XmlContent -> [XmlContent] -> XmlContent
addChildren (Xml.Element n as es1) es2 = Xml.Element n as (es1 ++ es2)
addChildren e _                        = e


