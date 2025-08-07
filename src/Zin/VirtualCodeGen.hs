{-# LANGUAGE OverloadedStrings #-}

module Zin.VirtualCodeGen
  ( generateVDOM,
    generateVDOMFromDocument,
    blockToVNode,
    styleToVNode,
    createDocumentVDOM,
  )
where

import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Zin.AST
import Zin.AST (getStyleRange)
import Zin.VirtualDOM (Attribute (..), VDocument (..), VNode (..), VProps (..), addAttribute, createElementNode, createRawTextNode, createTextNode, createVNode)
import qualified Zin.VirtualDOM as VD

-- Generate virtual DOM from Document
generateVDOMFromDocument :: Document -> VDocument
generateVDOMFromDocument = generateVDOM

generateVDOM :: Document -> VDocument
generateVDOM (Document blocks) =
  let bodyNode = createElementNode "div" [] (map blockToVNode blocks)
   in VDocument
        { vDocumentTitle = "Zin Document",
          vDocumentMeta = Map.fromList [("charset", "utf-8")],
          vDocumentBody = bodyNode
        }

-- Create a complete HTML document virtual DOM
createDocumentVDOM :: Document -> VNode
createDocumentVDOM (Document blocks) =
  createElementNode
    "html"
    []
    [ createElementNode
        "head"
        []
        [ createElementNode "meta" [Attribute "charset" "utf-8"] [],
          createElementNode "title" [] [createTextNode "Zin Document"]
        ],
      createElementNode "body" [] (map blockToVNode blocks)
    ]

-- Convert AST blocks to virtual DOM nodes
blockToVNode :: Block -> VNode
blockToVNode (Paragraph styles text) =
  let styledContent = applyStylesToVNode styles text
   in createElementNode "p" [] styledContent
blockToVNode (Header level styles text) =
  let headerFunc = getHeaderFunction level
      styledContent = applyStylesToVNode styles text
   in headerFunc [] styledContent
blockToVNode (List listType items) =
  let listFunc = getListFunction listType
      itemNodes = map listItemToVNode items
   in listFunc (getListAttributes listType) itemNodes
blockToVNode (CodeBlock lang code) =
  createElementNode
    "pre"
    []
    [ createElementNode
        "code"
        (if T.null lang then [] else [Attribute "class" ("language-" <> lang)])
        [createRawTextNode code]
    ]
  where
    codeAttrs =
      if T.null lang
        then []
        else [Attribute "class" ("language-" <> lang)]
blockToVNode (Quote styles text) =
  let styledContent = applyStylesToVNode styles text
   in createElementNode "blockquote" [] styledContent
blockToVNode (Table tables) =
  createElementNode "table" [] (map tableRowToVNode tables)

-- Helper functions for different element types
getHeaderFunction :: Int -> [Attribute] -> [VNode] -> VNode
getHeaderFunction 1 = createElementNode "h1"
getHeaderFunction 2 = createElementNode "h2"
getHeaderFunction 3 = createElementNode "h3"
getHeaderFunction 4 = createElementNode "h4"
getHeaderFunction _ = createElementNode "h1" -- Default to h1

getListFunction :: ListType -> [Attribute] -> [VNode] -> VNode
getListFunction Unordered = createElementNode "ul"
getListFunction Ordered = createElementNode "ol"
getListFunction Todo = createElementNode "ul" -- Todo lists are ul with special class

getListAttributes :: ListType -> [Attribute]
getListAttributes Todo = [Attribute "class" "todo-list"]
getListAttributes _ = []

listItemToVNode :: ListItem -> VNode
listItemToVNode (ListItem styles text) =
  let styledContent = applyStylesToVNode styles text
      attrs = case hasTodoStyle styles of
        True -> [Attribute "class" "todo-item"]
        False -> []
   in createElementNode "li" attrs styledContent
  where
    hasTodoStyle _ = False -- TODO: Implement todo detection

tableRowToVNode :: Table -> VNode
tableRowToVNode (HeaderRow cells) =
  createElementNode "tr" [] (map (\cell -> createElementNode "th" [] [createTextNode cell]) cells)
tableRowToVNode (DataRow cells) =
  createElementNode "tr" [] (map (\cell -> createElementNode "td" [] [createTextNode cell]) cells)

-- Style application for virtual DOM
applyStylesToVNode :: [Style] -> Text -> [VNode]
applyStylesToVNode [] text = [createTextNode text]
applyStylesToVNode styles text =
  let sortedStyles = sortBy (comparing getStyleStart) styles
   in applyStylesSequentially sortedStyles text 0

applyStylesSequentially :: [Style] -> Text -> Int -> [VNode]
applyStylesSequentially [] text offset = [createTextNode (T.drop offset text)]
applyStylesSequentially (style : restStyles) text offset =
  let (start, end) = getStyleRange style
      adjustedStart = max start offset
      adjustedEnd = min end (T.length text)
   in if adjustedStart >= adjustedEnd || adjustedStart < offset
        then applyStylesSequentially restStyles text offset
        else
          let beforeText = T.take (adjustedStart - offset) (T.drop offset text)
              styledText = T.take (adjustedEnd - adjustedStart) (T.drop adjustedStart text)
              beforeNode = if T.null beforeText then [] else [createTextNode beforeText]
              styledNode = styleToVNode style styledText
              afterNodes = applyStylesSequentially restStyles text adjustedEnd
           in beforeNode ++ [styledNode] ++ afterNodes

getStyleStart :: Style -> Int
getStyleStart style = fst (getStyleRange style)

-- Convert individual styles to virtual DOM nodes
styleToVNode :: Style -> Text -> VNode
styleToVNode (Bold _) text = createElementNode "strong" [] [createTextNode text]
styleToVNode (Italic _) text = createElementNode "em" [] [createTextNode text]
styleToVNode (Strike _) text = createElementNode "del" [] [createTextNode text]
styleToVNode (Link _ url) text = createElementNode "a" [Attribute "href" url] [createTextNode text]

-- Alternative approach: Apply styles with proper nesting
applyNestedStylesToVNode :: [Style] -> Text -> [VNode]
applyNestedStylesToVNode styles text =
  let styleRanges = map (\s -> (getStyleRange s, s)) styles
      sortedRanges = sortBy (comparing fst) styleRanges
      groupedStyles = groupNonOverlappingStyles sortedRanges
   in applyStyleGroups groupedStyles text 0

groupNonOverlappingStyles :: [((Int, Int), Style)] -> [[(Int, Int, Style)]]
groupNonOverlappingStyles [] = []
groupNonOverlappingStyles [((start, end), style)] = [[(start, end, style)]]
groupNonOverlappingStyles (((s1, e1), style1) : ((s2, e2), style2) : rest) =
  if e1 <= s2
    then [(s1, e1, style1)] : groupNonOverlappingStyles (((s2, e2), style2) : rest)
    else case groupNonOverlappingStyles (((s2, e2), style2) : rest) of
      [] -> [[(s1, e1, style1)]]
      (group : groups) -> ((s1, e1, style1) : group) : groups

applyStyleGroups :: [[(Int, Int, Style)]] -> Text -> Int -> [VNode]
applyStyleGroups [] text offset = [createTextNode (T.drop offset text)]
applyStyleGroups (group : groups) text offset =
  let (minStart, maxEnd) = getGroupBounds group
      beforeText = T.take (minStart - offset) (T.drop offset text)
      beforeNode = if T.null beforeText then [] else [createTextNode beforeText]
      groupNode = applyStyleGroup group text
      afterNodes = applyStyleGroups groups text maxEnd
   in beforeNode ++ [groupNode] ++ afterNodes

getGroupBounds :: [(Int, Int, Style)] -> (Int, Int)
getGroupBounds group =
  let starts = map (\(s, _, _) -> s) group
      ends = map (\(_, e, _) -> e) group
   in (minimum starts, maximum ends)

applyStyleGroup :: [(Int, Int, Style)] -> Text -> VNode
applyStyleGroup [(start, end, style)] text =
  let styledText = T.take (end - start) (T.drop start text)
   in styleToVNode style styledText
applyStyleGroup styles text =
  let (start, end, outerStyle) = head styles -- Simplified: apply first style as outer
      innerStyles = tail styles
      styledText = T.take (end - start) (T.drop start text)
      innerNode =
        if null innerStyles
          then createTextNode styledText
          else applyStyleGroup innerStyles text
   in wrapWithStyle outerStyle innerNode

wrapWithStyle :: Style -> VNode -> VNode
wrapWithStyle (Bold _) node = createElementNode "strong" [] [node]
wrapWithStyle (Italic _) node = createElementNode "em" [] [node]
wrapWithStyle (Strike _) node = createElementNode "del" [] [node]
wrapWithStyle (Link _ url) node = createElementNode "a" [Attribute "href" url] [node]

-- Utility functions for enhanced virtual DOM generation
createStyledElement :: Text -> [Style] -> Text -> VNode
createStyledElement tagName styles content =
  let attrs = stylesToAttributes styles
      textContent = [createTextNode content]
   in createElementNode tagName attrs textContent

stylesToAttributes :: [Style] -> [Attribute]
stylesToAttributes styles =
  let styleMap = Map.fromList $ concatMap styleToCSS styles
      styleAttr =
        if Map.null styleMap
          then []
          else [Attribute "style" (renderCSSMap styleMap)]
   in styleAttr

styleToCSS :: Style -> [(Text, Text)]
styleToCSS (Bold _) = [("font-weight", "bold")]
styleToCSS (Italic _) = [("font-style", "italic")]
styleToCSS (Strike _) = [("text-decoration", "line-through")]
styleToCSS (Link _ _) = [("color", "blue"), ("text-decoration", "underline")]

renderCSSMap :: Map.Map Text Text -> Text
renderCSSMap cssMap =
  T.intercalate "; " $ map (\(prop, val) -> prop <> ": " <> val) (Map.toList cssMap)

-- Enhanced document generation with metadata
generateVDOMWithMeta :: Document -> Text -> Map.Map Text Text -> VDocument
generateVDOMWithMeta doc title meta =
  let baseDom = generateVDOM doc
   in baseDom {vDocumentTitle = title, vDocumentMeta = meta}

-- Utility functions
escapeHTML :: Text -> Text
escapeHTML =
  T.replace "&" "&amp;"
    . T.replace "<" "&lt;"
    . T.replace ">" "&gt;"
    . T.replace "\"" "&quot;"
    . T.replace "'" "&#39;"

-- Component-based helpers for common patterns
createCodeBlockComponent :: Text -> Text -> VNode
createCodeBlockComponent lang code =
  let attrs =
        if T.null lang
          then [Attribute "class" "code-block"]
          else [Attribute "class" ("code-block language-" <> lang)]
   in createElementNode
        "div"
        attrs
        [ createElementNode
            "pre"
            []
            [createElementNode "code" [] [createTextNode code]]
        ]

createQuoteComponent :: [Style] -> Text -> VNode
createQuoteComponent styles text =
  let styledContent = applyStylesToVNode styles text
      attrs = [Attribute "class" "quote"]
   in createElementNode "blockquote" attrs styledContent

createTableComponent :: [Table] -> VNode
createTableComponent tables =
  let attrs = [Attribute "class" "zin-table"]
      rows = map tableRowToVNode tables
   in createElementNode "table" attrs rows