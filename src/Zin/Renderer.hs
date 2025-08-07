{-# LANGUAGE OverloadedStrings #-}

module Zin.Renderer
  ( RenderEngine (..),
    RenderContext (..),
    DOMOperation (..),
    renderToHTML,
    renderToString,
    renderVDocument,
    createRenderEngine,
    executeRender,
    renderWithContext,
    generateDOMOperations,
    optimizeRender,
  )
where

import Control.Monad (unless)
import Control.Monad.State
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Zin.AST (Document)
import Zin.VirtualCodeGen
import Zin.VirtualDOM

-- Render Engine with state management
data RenderEngine = RenderEngine
  { engineState :: Map Text Text,
    engineOptions :: RenderOptions,
    engineCache :: Map Text VNode
  }
  deriving (Show, Eq)

data RenderOptions = RenderOptions
  { optimizeOutput :: Bool,
    enableCaching :: Bool,
    includeDataAttributes :: Bool,
    enableEventHandlers :: Bool,
    minifyHTML :: Bool
  }
  deriving (Show, Eq)

defaultRenderOptions :: RenderOptions
defaultRenderOptions =
  RenderOptions
    { optimizeOutput = True,
      enableCaching = False,
      includeDataAttributes = True,
      enableEventHandlers = False,
      minifyHTML = False
    }

-- Render Context for stateful operations
data RenderContext = RenderContext
  { contextState :: Map Text Text,
    contextDepth :: Int,
    contextParent :: Maybe Text,
    contextEvents :: [EventBinding]
  }
  deriving (Show, Eq)

data EventBinding = EventBinding
  { eventBindingId :: Text,
    eventBindingType :: Text,
    eventBindingTarget :: Text
  }
  deriving (Show, Eq)

-- DOM Operations for client-side rendering
data DOMOperation
  = CreateElement Text Text (Map Text Text)
  | CreateTextNode Text Text
  | SetAttribute Text Text Text
  | SetProperty Text Text Text
  | SetEventListener Text Text Text
  | AppendChild Text Text
  | RemoveChild Text Text
  | UpdateTextContent Text Text
  deriving (Show, Eq)

-- Core rendering functions
createRenderEngine :: RenderOptions -> RenderEngine
createRenderEngine options =
  RenderEngine
    { engineState = Map.empty,
      engineOptions = options,
      engineCache = Map.empty
    }

renderToHTML :: VDocument -> Text
renderToHTML vdoc =
  let engine = createRenderEngine defaultRenderOptions
   in executeRender engine (renderVDocument vdoc)

renderToString :: VNode -> Text
renderToString vnode =
  let engine = createRenderEngine defaultRenderOptions
   in executeRender engine (renderVNodeWithContext vnode)

renderVDocument :: VDocument -> State RenderEngine Text
renderVDocument (VDocument title meta body) = do
  bodyHTML <- renderVNodeWithContext body
  let metaTags = Map.foldlWithKey (\acc k v -> acc <> "<meta name=\"" <> escapeHTML k <> "\" content=\"" <> escapeHTML v <> "\">\n") "" meta
  return $
    T.concat
      [ "<!DOCTYPE html>\n",
        "<html>\n",
        "<head>\n",
        metaTags,
        "<title>" <> escapeHTML title <> "</title>\n",
        "</head>\n",
        "<body>\n",
        bodyHTML,
        "</body>\n",
        "</html>"
      ]

executeRender :: RenderEngine -> State RenderEngine a -> a
executeRender engine computation = evalState computation engine

renderWithContext :: RenderContext -> VNode -> State RenderEngine Text
renderWithContext context vnode = do
  engine <- get
  let options = engineOptions engine
  case vnode of
    VText text -> return (escapeHTML text)
    VRawText text -> return text
    VElement tag props children key -> do
      let elementId = generateElementId context tag key
      openTag <- renderOpenTag tag props elementId options
      childrenHTML <- mapM (renderWithContext (updateContext context elementId)) children
      let closeTag = "</" <> tag <> ">"
      return $ openTag <> T.concat childrenHTML <> closeTag
    VComponent name props children -> do
      -- Component rendering (could be expanded for custom components)
      childrenHTML <- mapM (renderWithContext context) children
      return $ T.concat childrenHTML

renderVNodeWithContext :: VNode -> State RenderEngine Text
renderVNodeWithContext vnode = do
  let initialContext = RenderContext Map.empty 0 Nothing []
  renderWithContext initialContext vnode

generateElementId :: RenderContext -> Text -> Maybe Text -> Text
generateElementId context tag key =
  let depth = T.pack $ show $ contextDepth context
      baseId = case key of
        Just k -> tag <> "-" <> k
        Nothing -> tag <> "-" <> depth
   in case contextParent context of
        Just parent -> parent <> "-" <> baseId
        Nothing -> baseId

updateContext :: RenderContext -> Text -> RenderContext
updateContext context elementId =
  context
    { contextDepth = contextDepth context + 1,
      contextParent = Just elementId
    }

renderOpenTag :: Text -> VProps -> Text -> RenderOptions -> State RenderEngine Text
renderOpenTag tag props elementId options = do
  let VProps attrs events styles className = props
  attrsHTML <- renderAttributes attrs options
  stylesHTML <- renderStylesHTML styles
  classHTML <- renderClassHTML className
  eventsHTML <-
    if enableEventHandlers options
      then renderEventsHTML events elementId
      else return ""
  dataAttrsHTML <-
    if includeDataAttributes options
      then return $ " data-vdom-id=\"" <> escapeHTML elementId <> "\""
      else return ""

  let allAttrs = T.concat [attrsHTML, stylesHTML, classHTML, eventsHTML, dataAttrsHTML]
  return $ "<" <> tag <> allAttrs <> ">"

renderAttributes :: Map Text Text -> RenderOptions -> State RenderEngine Text
renderAttributes attrs options =
  return $ Map.foldlWithKey (\acc k v -> acc <> " " <> k <> "=\"" <> escapeHTML v <> "\"") "" attrs

renderStylesHTML :: Map Text Text -> State RenderEngine Text
renderStylesHTML styles =
  if Map.null styles
    then return ""
    else do
      let styleText =
            Map.foldlWithKey
              ( \acc k v ->
                  if T.null acc then k <> ": " <> v else acc <> "; " <> k <> ": " <> v
              )
              ""
              styles
      return $ " style=\"" <> escapeHTML styleText <> "\""

renderClassHTML :: Maybe Text -> State RenderEngine Text
renderClassHTML Nothing = return ""
renderClassHTML (Just className) = return $ " class=\"" <> escapeHTML className <> "\""

renderEventsHTML :: Map Text EventHandler -> Text -> State RenderEngine Text
renderEventsHTML events elementId = do
  -- Generate event binding attributes (for client-side JavaScript)
  let eventAttrs =
        Map.foldlWithKey
          ( \acc eventType _ ->
              acc <> " data-event-" <> eventType <> "=\"" <> elementId <> "\""
          )
          ""
          events
  -- Store event bindings for later JavaScript generation
  engine <- get
  let newEvents =
        Map.foldlWithKey
          ( \acc eventType' (EventHandler eType _) ->
              EventBinding elementId eventType' eType : acc
          )
          []
          events
  -- Note: In a real implementation, we'd store these events for client-side binding
  return eventAttrs

-- DOM Operations generation for client-side rendering
generateDOMOperations :: VNode -> [DOMOperation]
generateDOMOperations vnode = execWriter (generateOpsForNode vnode Nothing)

generateOpsForNode :: VNode -> Maybe Text -> Writer [DOMOperation] Text
generateOpsForNode (VText text) parentId = do
  let nodeId = "text-" <> T.take 8 (T.filter (/= ' ') text)
  tell [CreateTextNode nodeId (escapeHTML text)]
  case parentId of
    Just pid -> tell [AppendChild pid nodeId]
    Nothing -> return ()
  return nodeId
generateOpsForNode (VRawText text) parentId = do
  let nodeId = "text-" <> T.take 8 (T.filter (/= ' ') text)
  tell [CreateTextNode nodeId text]
  case parentId of
    Just pid -> tell [AppendChild pid nodeId]
    Nothing -> return ()
  return nodeId
generateOpsForNode (VElement tag props children key) parentId = do
  let nodeId = case key of
        Just k -> tag <> "-" <> k
        Nothing -> tag <> "-gen"
      VProps attrs events styles className = props

  tell [CreateElement nodeId tag attrs]

  -- Set styles
  unless (Map.null styles) $
    mapM_ (\(prop, val) -> tell [SetProperty nodeId ("style." <> prop) val]) (Map.toList styles)

  -- Set class
  case className of
    Just cls -> tell [SetAttribute nodeId "class" cls]
    Nothing -> return ()

  -- Set event listeners
  mapM_ (\(eventType, _) -> tell [SetEventListener nodeId eventType nodeId]) (Map.toList events)

  -- Append to parent
  case parentId of
    Just pid -> tell [AppendChild pid nodeId]
    Nothing -> return ()

  -- Process children
  mapM_ (\child -> generateOpsForNode child (Just nodeId)) children

  return nodeId
generateOpsForNode (VComponent name props children) parentId = do
  -- For components, we render them as div containers for now
  generateOpsForNode (VElement "div" props children Nothing) parentId

-- Optimization functions
optimizeRender :: VNode -> VNode
optimizeRender = optimizeTextNodes . removeEmptyNodes . mergeAdjacentText

removeEmptyNodes :: VNode -> VNode
removeEmptyNodes (VText text) = if T.null (T.strip text) then VText "" else VText text
removeEmptyNodes (VRawText text) = if T.null (T.strip text) then VRawText "" else VRawText text
removeEmptyNodes (VElement tag props children key) =
  let filteredChildren = filter (not . isEmptyNode) (map removeEmptyNodes children)
   in VElement tag props filteredChildren key
removeEmptyNodes (VComponent name props children) =
  let filteredChildren = filter (not . isEmptyNode) (map removeEmptyNodes children)
   in VComponent name props filteredChildren

isEmptyNode :: VNode -> Bool
isEmptyNode (VText text) = T.null (T.strip text)
isEmptyNode _ = False

mergeAdjacentText :: VNode -> VNode
mergeAdjacentText (VElement tag props children key) =
  VElement tag props (mergeTextNodes children) key
mergeAdjacentText (VComponent name props children) =
  VComponent name props (mergeTextNodes children)
mergeAdjacentText node = node

mergeTextNodes :: [VNode] -> [VNode]
mergeTextNodes [] = []
mergeTextNodes [node] = [mergeAdjacentText node]
mergeTextNodes (VText t1 : VText t2 : rest) = mergeTextNodes (VText (t1 <> t2) : rest)
mergeTextNodes (node : rest) = mergeAdjacentText node : mergeTextNodes rest

optimizeTextNodes :: VNode -> VNode
optimizeTextNodes (VText text) = VText (T.strip text)
optimizeTextNodes (VRawText text) = VRawText text -- Don't strip raw text in code blocks
optimizeTextNodes (VElement tag props children key) =
  VElement tag props (map optimizeTextNodes children) key
optimizeTextNodes (VComponent name props children) =
  VComponent name props (map optimizeTextNodes children)

-- Utility functions
escapeHTML :: Text -> Text
escapeHTML =
  T.replace "&" "&amp;"
    . T.replace "<" "&lt;"
    . T.replace ">" "&gt;"
    . T.replace "\"" "&quot;"
    . T.replace "'" "&#39;"

-- Client-side JavaScript generation
generateClientScript :: [EventBinding] -> Text
generateClientScript events =
  let eventBindings = map generateEventBinding events
   in T.concat
        [ "<script>\n",
          "// Virtual DOM Event Bindings\n",
          T.concat eventBindings,
          "</script>\n"
        ]

generateEventBinding :: EventBinding -> Text
generateEventBinding (EventBinding elementId eventType handler) =
  T.concat
    [ "document.querySelector('[data-vdom-id=\"",
      elementId,
      "\"]').addEventListener('",
      eventType,
      "', ",
      handler,
      ");\n"
    ]

-- Performance monitoring
data RenderStats = RenderStats
  { renderTime :: Double,
    nodesRendered :: Int,
    cacheHits :: Int,
    cacheMisses :: Int
  }
  deriving (Show, Eq)

-- Advanced rendering with caching
renderWithCache :: Map Text VNode -> VNode -> (Text, Map Text VNode)
renderWithCache cache vnode =
  let engine = createRenderEngine defaultRenderOptions {enableCaching = True}
      engine' = engine {engineCache = cache}
      result = executeRender engine' (renderVNodeWithContext vnode)
      newCache = engineCache $ execState (renderVNodeWithContext vnode) engine'
   in (result, newCache)