{-# LANGUAGE OverloadedStrings #-}

module Zin.AST
  ( Document (..),
    Block (..),
    Style (..),
    ListType (..),
    ListItem (..),
    Table (..),
    getHeaderLevel,
    getListType,
    isTopLevelTag,
    isHeaderTag,
    isListTag,
    isTableTag,
    validateStyleRange,
    getStyleRange,
    applyStyleToText,
  )
where

import Data.Text (Text)
import qualified Data.Text as T

newtype Document = Document [Block]

data Block
  = Paragraph [Style] Text
  | Header Int [Style] Text
  | List ListType [ListItem]
  | CodeBlock Text Text
  | Quote [Style] Text
  | Table [Table]
  deriving (Show, Eq)

data Style
  = Bold (Int, Int)
  | Italic (Int, Int)
  | Strike (Int, Int)
  | Link (Int, Int) Text
  deriving (Show, Eq)

data ListType
  = Unordered
  | Ordered
  | Todo
  deriving (Show, Eq)

data ListItem = ListItem [Style] Text
  deriving (Show, Eq)

data Table
  = HeaderRow [Text]
  | DataRow [Text]
  deriving (Show, Eq)

-- Helper functions for AST manipulation
getHeaderLevel :: Text -> Maybe Int
getHeaderLevel "h1" = Just 1
getHeaderLevel "h2" = Just 2
getHeaderLevel "h3" = Just 3
getHeaderLevel "h4" = Just 4
getHeaderLevel _ = Nothing

getListType :: Text -> Maybe ListType
getListType "ul" = Just Unordered
getListType "ol" = Just Ordered
getListType "tl" = Just Todo
getListType _ = Nothing

isTopLevelTag :: Text -> Bool
isTopLevelTag tag = tag `elem` ["p", "h1", "h2", "h3", "h4", "ul", "ol", "tl", "cb", "q", "t", "r"]

isHeaderTag :: Text -> Bool
isHeaderTag tag = tag `elem` ["h1", "h2", "h3", "h4"]

isListTag :: Text -> Bool
isListTag tag = tag `elem` ["ul", "ol", "tl"]

isTableTag :: Text -> Bool
isTableTag tag = tag `elem` ["t", "r"]

-- Style validation helpers
validateStyleRange :: Style -> Text -> Bool
validateStyleRange style text =
  let len = T.length text
      (start, end) = getStyleRange style
   in start >= 0 && end <= len && start <= end

getStyleRange :: Style -> (Int, Int)
getStyleRange (Bold range) = range
getStyleRange (Italic range) = range
getStyleRange (Strike range) = range
getStyleRange (Link range _) = range

-- Text manipulation for styles
applyStyleToText :: Style -> Text -> Text
applyStyleToText (Bold (s, e)) text =
  let (before, rest) = T.splitAt s text
      (middle, after) = T.splitAt (e - s) rest
   in before <> "<strong>" <> middle <> "</strong>" <> after
applyStyleToText (Italic (s, e)) text =
  let (before, rest) = T.splitAt s text
      (middle, after) = T.splitAt (e - s) rest
   in before <> "<em>" <> middle <> "</em>" <> after
applyStyleToText (Strike (s, e)) text =
  let (before, rest) = T.splitAt s text
      (middle, after) = T.splitAt (e - s) rest
   in before <> "<del>" <> middle <> "</del>" <> after
applyStyleToText (Link (s, e) url) text =
  let (before, rest) = T.splitAt s text
      (middle, after) = T.splitAt (e - s) rest
   in before <> "<a href=\"" <> url <> "\">" <> middle <> "</a>" <> after