{-# LANGUAGE OverloadedStrings #-}

module Zin.VirtualDOM 
  ( VNode(..)
  , VProps(..)
  , VEvent(..)
  , VDocument(..)
  , Attribute(..)
  , EventHandler(..)
  , createVNode
  , createTextNode
  , createRawTextNode
  , createElementNode
  , addAttribute
  , addEventListener
  , renderVNode
  , diffVNodes
  , applyPatches
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)

-- Virtual DOM Node representation
data VNode
  = VElement 
    { vTag :: Text
    , vProps :: VProps
    , vChildren :: [VNode]
    , vKey :: Maybe Text
    }
  | VText Text
  | VRawText Text  -- For code blocks where we don't want HTML escaping
  | VComponent 
    { vComponentName :: Text
    , vComponentProps :: VProps
    , vComponentChildren :: [VNode]
    }
  deriving (Show, Eq)

-- Properties and attributes for virtual nodes
data VProps = VProps
  { vAttributes :: Map Text Text
  , vEvents :: Map Text EventHandler
  , vStyles :: Map Text Text
  , vClassName :: Maybe Text
  } deriving (Show, Eq)

-- Event handling system
data EventHandler = EventHandler
  { eventType :: Text
  , handler :: Text -> IO ()
  } 

instance Show EventHandler where
  show (EventHandler eType _) = "EventHandler(" <> T.unpack eType <> ")"

instance Eq EventHandler where
  (EventHandler t1 _) == (EventHandler t2 _) = t1 == t2

-- Document-level virtual DOM
data VDocument = VDocument
  { vDocumentTitle :: Text
  , vDocumentMeta :: Map Text Text
  , vDocumentBody :: VNode
  } deriving (Show, Eq)

-- Attribute helper type
data Attribute = Attribute Text Text deriving (Show, Eq)

-- Event types for the zin language
data VEvent 
  = OnClick
  | OnHover
  | OnFocus
  | OnBlur
  | OnChange
  deriving (Show, Eq)

-- Core virtual DOM creation functions
createVNode :: Text -> VProps -> [VNode] -> VNode
createVNode tag props children = VElement tag props children Nothing

createTextNode :: Text -> VNode
createTextNode = VText

createRawTextNode :: Text -> VNode
createRawTextNode = VRawText

createElementNode :: Text -> [Attribute] -> [VNode] -> VNode
createElementNode tag attrs children = 
  let props = foldl addAttributeToProps emptyProps attrs
  in VElement tag props children Nothing
  where
    addAttributeToProps props (Attribute key value) = addAttribute key value props

-- Props manipulation
emptyProps :: VProps
emptyProps = VProps Map.empty Map.empty Map.empty Nothing

addAttribute :: Text -> Text -> VProps -> VProps
addAttribute key value props = 
  props { vAttributes = Map.insert key value (vAttributes props) }

addStyle :: Text -> Text -> VProps -> VProps
addStyle property value props = 
  props { vStyles = Map.insert property value (vStyles props) }

setClassName :: Text -> VProps -> VProps
setClassName className props = props { vClassName = Just className }

addEventListener :: Text -> EventHandler -> VProps -> VProps
addEventListener eventType handler props = 
  props { vEvents = Map.insert eventType handler (vEvents props) }

-- Virtual DOM to DOM rendering (conceptual - would interface with actual DOM)
renderVNode :: VNode -> Text
renderVNode (VText text) = escapeHTML text
renderVNode (VRawText text) = text
renderVNode (VElement tag props children Nothing) = 
  let openTag = "<" <> tag <> renderProps props <> ">"
      closeTag = "</" <> tag <> ">"
      childrenHTML = T.concat (map renderVNode children)
  in openTag <> childrenHTML <> closeTag
renderVNode (VElement tag props children (Just key)) = 
  let openTag = "<" <> tag <> " data-key=\"" <> escapeHTML key <> "\"" <> renderProps props <> ">"
      closeTag = "</" <> tag <> ">"
      childrenHTML = T.concat (map renderVNode children)
  in openTag <> childrenHTML <> closeTag
renderVNode (VComponent name props children) = 
  let openTag = "<" <> name <> renderProps props <> ">"
      closeTag = "</" <> name <> ">"
      childrenHTML = T.concat (map renderVNode children)
  in openTag <> childrenHTML <> closeTag

renderProps :: VProps -> Text
renderProps (VProps attrs _ styles className) = 
  let attrText = Map.foldlWithKey renderAttribute "" attrs
      styleText = if Map.null styles then "" else " style=\"" <> renderStyles styles <> "\""
      classText = case className of
        Nothing -> ""
        Just cls -> " class=\"" <> escapeHTML cls <> "\""
  in attrText <> styleText <> classText

renderAttribute :: Text -> Text -> Text -> Text
renderAttribute acc key value = acc <> " " <> key <> "=\"" <> escapeHTML value <> "\""

renderStyles :: Map Text Text -> Text
renderStyles styles = 
  T.intercalate "; " $ map (\(prop, val) -> prop <> ": " <> val) (Map.toList styles)

-- Virtual DOM diffing algorithm (simplified)
data Patch 
  = Replace VNode
  | UpdateProps VProps
  | UpdateChildren [ChildPatch]
  | Remove
  deriving (Show, Eq)

data ChildPatch 
  = ChildInsert Int VNode
  | ChildRemove Int
  | ChildUpdate Int Patch
  deriving (Show, Eq)

diffVNodes :: VNode -> VNode -> Maybe Patch
diffVNodes oldNode newNode
  | oldNode == newNode = Nothing
  | otherwise = case (oldNode, newNode) of
      (VText oldText, VText newText) -> 
        if oldText == newText then Nothing else Just (Replace newNode)
      (VRawText oldText, VRawText newText) -> 
        if oldText == newText then Nothing else Just (Replace newNode)
      (VElement oldTag oldProps oldChildren _, VElement newTag newProps newChildren _) -> 
        if oldTag /= newTag 
          then Just (Replace newNode)
          else 
            let propsPatch = if oldProps == newProps then Nothing else Just (UpdateProps newProps)
                childrenPatch = diffChildren oldChildren newChildren
            in case (propsPatch, childrenPatch) of
              (Nothing, []) -> Nothing
              (Just propsPatch', []) -> Just propsPatch'
              (Nothing, children) -> Just (UpdateChildren children)
              (Just propsPatch', children) -> Just propsPatch' -- Simplified: apply props first
      _ -> Just (Replace newNode)

diffChildren :: [VNode] -> [VNode] -> [ChildPatch]
diffChildren oldChildren newChildren = 
  let oldLen = length oldChildren
      newLen = length newChildren
      minLen = min oldLen newLen
      updates = [ChildUpdate i patch | (i, (old, new)) <- zip [0..] (zip oldChildren newChildren),
                                     Just patch <- [diffVNodes old new]]
      removes = [ChildRemove i | i <- [minLen..oldLen-1]]
      inserts = [ChildInsert i node | (i, node) <- zip [minLen..] (drop minLen newChildren)]
  in updates ++ removes ++ inserts

-- Apply patches to actual DOM (conceptual)
applyPatches :: VNode -> [Patch] -> VNode
applyPatches node patches = foldl applyPatch node patches

applyPatch :: VNode -> Patch -> VNode
applyPatch _ (Replace newNode) = newNode
applyPatch (VElement tag _ children key) (UpdateProps newProps) = 
  VElement tag newProps children key
applyPatch node _ = node -- Simplified implementation

-- Utility functions
escapeHTML :: Text -> Text
escapeHTML = T.replace "&" "&amp;" 
           . T.replace "<" "&lt;" 
           . T.replace ">" "&gt;" 
           . T.replace "\"" "&quot;" 
           . T.replace "'" "&#39;"

-- Helper functions for common element creation
h1 :: [Attribute] -> [VNode] -> VNode
h1 = createElementNode "h1"

h2 :: [Attribute] -> [VNode] -> VNode
h2 = createElementNode "h2"

h3 :: [Attribute] -> [VNode] -> VNode
h3 = createElementNode "h3"

h4 :: [Attribute] -> [VNode] -> VNode
h4 = createElementNode "h4"

p :: [Attribute] -> [VNode] -> VNode
p = createElementNode "p"

div :: [Attribute] -> [VNode] -> VNode
div = createElementNode "div"

span :: [Attribute] -> [VNode] -> VNode
span = createElementNode "span"

ul :: [Attribute] -> [VNode] -> VNode
ul = createElementNode "ul"

ol :: [Attribute] -> [VNode] -> VNode
ol = createElementNode "ol"

li :: [Attribute] -> [VNode] -> VNode
li = createElementNode "li"

table :: [Attribute] -> [VNode] -> VNode
table = createElementNode "table"

tr :: [Attribute] -> [VNode] -> VNode
tr = createElementNode "tr"

td :: [Attribute] -> [VNode] -> VNode
td = createElementNode "td"

th :: [Attribute] -> [VNode] -> VNode
th = createElementNode "th"

strong :: [Attribute] -> [VNode] -> VNode
strong = createElementNode "strong"

em :: [Attribute] -> [VNode] -> VNode
em = createElementNode "em"

del :: [Attribute] -> [VNode] -> VNode
del = createElementNode "del"

a :: [Attribute] -> [VNode] -> VNode
a = createElementNode "a"

blockquote :: [Attribute] -> [VNode] -> VNode
blockquote = createElementNode "blockquote"

pre :: [Attribute] -> [VNode] -> VNode
pre = createElementNode "pre"

code :: [Attribute] -> [VNode] -> VNode
code = createElementNode "code"