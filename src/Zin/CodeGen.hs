{-# LANGUAGE OverloadedStrings #-}

module Zin.CodeGen
  ( generateHTML,
    generateHTMLFromDocument,
    blockToHTML,
    styleToHTML,
  )
where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Zin.AST

generateHTMLFromDocument :: Document -> Text
generateHTMLFromDocument = generateHTML

generateHTML :: Document -> Text
generateHTML (Document blocks) =
  "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n<title>Zin Document</title>\n</head>\n<body>\n"
    <> T.concat (map blockToHTML blocks)
    <> "</body>\n</html>"

blockToHTML :: Block -> Text
blockToHTML (Paragraph styles text) =
  let styledText = applyStylesToText styles text
   in "<p>" <> escapeHTML styledText <> "</p>\n"
blockToHTML (Header level styles text) =
  let tag = "h" <> T.pack (show level)
      styledText = applyStylesToText styles text
   in "<" <> tag <> ">" <> escapeHTML styledText <> "</" <> tag <> ">\n"
blockToHTML (List listType items) =
  let tag = getListTag listType
      itemsHTML = T.concat (map listItemToHTML items)
   in "<" <> tag <> ">\n" <> itemsHTML <> "</" <> tag <> ">\n"
blockToHTML (CodeBlock lang code) =
  "<pre><code class=\"language-"
    <> escapeHTML lang
    <> "\">"
    <> escapeHTML code
    <> "</code></pre>\n"
blockToHTML (Quote styles text) =
  let styledText = applyStylesToText styles text
   in "<blockquote>" <> escapeHTML styledText <> "</blockquote>\n"
blockToHTML (Table tables) =
  "<table>\n" <> T.concat (map tableRowToHTML tables) <> "</table>\n"

listItemToHTML :: ListItem -> Text
listItemToHTML (ListItem styles text) =
  let styledText = applyStylesToText styles text
   in "<li>" <> escapeHTML styledText <> "</li>\n"

tableRowToHTML :: Table -> Text
tableRowToHTML (HeaderRow cells) =
  "<tr>\n" <> T.concat (map (\cell -> "<th>" <> escapeHTML cell <> "</th>\n") cells) <> "</tr>\n"
tableRowToHTML (DataRow cells) =
  "<tr>\n" <> T.concat (map (\cell -> "<td>" <> escapeHTML cell <> "</td>\n") cells) <> "</tr>\n"

getListTag :: ListType -> Text
getListTag Unordered = "ul"
getListTag Ordered = "ol"
getListTag Todo = "ul class=\"todo-list\""

applyStylesToText :: [Style] -> Text -> Text
applyStylesToText styles text =
  let sortedStyles = sortBy (comparing getStyleStart) styles
   in foldr (applyStyleSafely text) text sortedStyles

applyStyleSafely :: Text -> Style -> Text -> Text
applyStyleSafely originalText style currentText =
  let textLength = T.length originalText
      (start, end) = getStyleRange style
   in if start >= 0 && end <= textLength && start <= end
        then applyStyleToTextAt style start end currentText
        else currentText

applyStyleToTextAt :: Style -> Int -> Int -> Text -> Text
applyStyleToTextAt style start end text =
  let (before, rest) = T.splitAt start text
      (middle, after) = T.splitAt (end - start) rest
      styledMiddle = case style of
        Bold _ -> "<strong>" <> middle <> "</strong>"
        Italic _ -> "<em>" <> middle <> "</em>"
        Strike _ -> "<del>" <> middle <> "</del>"
        Link _ url -> "<a href=\"" <> escapeHTML url <> "\">" <> middle <> "</a>"
   in before <> styledMiddle <> after

getStyleStart :: Style -> Int
getStyleStart style = fst (getStyleRange style)

styleToHTML :: Style -> Text -> Text
styleToHTML (Bold (s, e)) text = insertTag "strong" s e text
styleToHTML (Italic (s, e)) text = insertTag "em" s e text
styleToHTML (Strike (s, e)) text = insertTag "del" s e text
styleToHTML (Link (s, e) url) text = insertTag ("a href=\"" <> escapeHTML url <> "\"") s e text

insertTag :: Text -> Int -> Int -> Text -> Text
insertTag tagName start end text =
  let (before, rest) = T.splitAt start text
      (middle, after) = T.splitAt (end - start) rest
   in before <> "<" <> tagName <> ">" <> middle <> "</" <> getTagName tagName <> ">" <> after

getTagName :: Text -> Text
getTagName tagWithAttrs = T.takeWhile (/= ' ') tagWithAttrs

escapeHTML :: Text -> Text
escapeHTML =
  T.replace "&" "&amp;"
    . T.replace "<" "&lt;"
    . T.replace ">" "&gt;"
    . T.replace "\"" "&quot;"
    . T.replace "'" "&#39;"

-- Advanced styling with proper nesting
applyNestedStyles :: [Style] -> Text -> Text
applyNestedStyles styles text =
  let sortedStyles = sortBy (comparing getStyleRange) styles
      groupedStyles = groupNonOverlapping sortedStyles
   in foldr applyStyleGroup text groupedStyles

groupNonOverlapping :: [Style] -> [[Style]]
groupNonOverlapping [] = []
groupNonOverlapping [s] = [[s]]
groupNonOverlapping (s1 : s2 : rest) =
  let (start1, end1) = getStyleRange s1
      (start2, _) = getStyleRange s2
   in if end1 <= start2
        then [s1] : groupNonOverlapping (s2 : rest)
        else case groupNonOverlapping (s2 : rest) of
          [] -> [[s1]]
          (group : groups) -> (s1 : group) : groups

applyStyleGroup :: [Style] -> Text -> Text
applyStyleGroup styles text = foldr applyStyleToTextAt' text styles
  where
    applyStyleToTextAt' style txt =
      let (start, end) = getStyleRange style
       in applyStyleToTextAt style start end txt

-- Utility for code block generation
generateCodeBlock :: Text -> Text -> Text
generateCodeBlock language code =
  "<pre><code class=\"language-"
    <> escapeHTML language
    <> "\">"
    <> escapeHTML code
    <> "</code></pre>"

-- CSS class generation for todo lists
generateTodoListHTML :: [ListItem] -> Text
generateTodoListHTML items =
  "<ul class=\"todo-list\">\n"
    <> T.concat (map todoItemToHTML items)
    <> "</ul>\n"
  where
    todoItemToHTML (ListItem styles text) =
      let styledText = applyStylesToText styles text
       in "<li class=\"todo-item\">" <> escapeHTML styledText <> "</li>\n"