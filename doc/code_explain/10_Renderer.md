# Renderer.hs - 高度なレンダリングエンジン

## 概要

`Renderer.hs`は、仮想DOMから最終的なHTML出力を生成する**高度なレンダリングエンジン**を実装するモジュールです。状態管理、イベントハンドリング、最適化、キャッシュなど、本格的なWebフレームワークレベルの機能を提供します。

## 主要なデータ型

### `RenderEngine` - レンダリングエンジン
```haskell
data RenderEngine = RenderEngine
  { engineState :: Map Text Text
  , engineOptions :: RenderOptions
  , engineCache :: Map Text VNode
  } deriving (Show, Eq)
```

レンダリングエンジンの核となるデータ構造：
- **`engineState`**: グローバル状態管理
- **`engineOptions`**: レンダリング設定
- **`engineCache`**: パフォーマンス向上のためのキャッシュ

### `RenderOptions` - レンダリング設定
```haskell
data RenderOptions = RenderOptions
  { optimizeOutput :: Bool
  , enableCaching :: Bool
  , includeDataAttributes :: Bool
  , enableEventHandlers :: Bool
  , minifyHTML :: Bool
  } deriving (Show, Eq)

defaultRenderOptions :: RenderOptions
defaultRenderOptions = RenderOptions
  { optimizeOutput = True
  , enableCaching = False
  , includeDataAttributes = True
  , enableEventHandlers = False
  , minifyHTML = False
  }
```

レンダリング動作を制御する設定群：
- **`optimizeOutput`**: 出力最適化の有効化
- **`enableCaching`**: キャッシュ機能の有効化
- **`includeDataAttributes`**: データ属性の挿入
- **`enableEventHandlers`**: イベントハンドラーのサポート
- **`minifyHTML`**: HTML最小化

### `RenderContext` - レンダリングコンテキスト
```haskell
data RenderContext = RenderContext
  { contextState :: Map Text Text
  , contextDepth :: Int
  , contextParent :: Maybe Text
  , contextEvents :: [EventBinding]
  } deriving (Show, Eq)
```

レンダリング中の文脈情報：
- **`contextState`**: 局所的な状態
- **`contextDepth`**: ネストの深さ
- **`contextParent`**: 親要素のID
- **`contextEvents`**: イベントバインディング

### `DOMOperation` - DOM操作
```haskell
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
```

クライアントサイドでのDOM操作を表現。将来的なSPA（Single Page Application）サポートの基盤。

## 主要な関数

### エンジン作成と実行

#### `createRenderEngine :: RenderOptions -> RenderEngine`
```haskell
createRenderEngine options = RenderEngine
  { engineState = Map.empty
  , engineOptions = options
  , engineCache = Map.empty
  }
```

#### `executeRender :: RenderEngine -> State RenderEngine a -> a`
```haskell
executeRender engine computation = evalState computation engine
```

### メインレンダリング関数

#### `renderToHTML :: VDocument -> Text`
```haskell
renderToHTML vdoc = 
  let engine = createRenderEngine defaultRenderOptions
  in executeRender engine (renderVDocument vdoc)
```

仮想ドキュメントを完全なHTMLに変換。

#### `renderVDocument :: VDocument -> State RenderEngine Text`
```haskell
renderVDocument (VDocument title meta body) = do
  bodyHTML <- renderVNodeWithContext body
  let metaTags = Map.foldlWithKey (\acc k v -> acc <> "<meta name=\"" <> escapeHTML k <> "\" content=\"" <> escapeHTML v <> "\">\n") "" meta
  return $ T.concat
    [ "<!DOCTYPE html>\n"
    , "<html>\n"
    , "<head>\n"
    , metaTags
    , "<title>" <> escapeHTML title <> "</title>\n"
    , "</head>\n"
    , "<body>\n"
    , bodyHTML
    , "</body>\n"
    , "</html>"
    ]
```

完全なHTML文書構造を生成。メタタグとタイトルを適切に挿入。

### コンテキスト管理レンダリング

#### `renderWithContext :: RenderContext -> VNode -> State RenderEngine Text`
```haskell
renderWithContext context vnode = do
  engine <- get
  let options = engineOptions engine
  case vnode of
    VText text -> return (escapeHTML text)
    VElement tag props children key -> do
      let elementId = generateElementId context tag key
      openTag <- renderOpenTag tag props elementId options
      childrenHTML <- mapM (renderWithContext (updateContext context elementId)) children
      let closeTag = "</" <> tag <> ">"
      return $ openTag <> T.concat childrenHTML <> closeTag
    VComponent name props children -> do
      childrenHTML <- mapM (renderWithContext context) children
      return $ T.concat childrenHTML
```

文脈情報を管理しながらレンダリング：
1. **要素ID生成**: 一意識別子を自動生成
2. **開始タグ**: 属性とプロパティを含むタグ生成
3. **子要素処理**: 再帰的に子要素をレンダリング
4. **文脈更新**: ネストレベルと親要素情報を更新

### ID生成と文脈更新

#### `generateElementId :: RenderContext -> Text -> Maybe Text -> Text`
```haskell
generateElementId context tag key = 
  let depth = T.pack $ show $ contextDepth context
      baseId = case key of
        Just k -> tag <> "-" <> k
        Nothing -> tag <> "-" <> depth
  in case contextParent context of
    Just parent -> parent <> "-" <> baseId
    Nothing -> baseId
```

階層構造を反映した一意のID生成。

#### `updateContext :: RenderContext -> Text -> RenderContext`
```haskell
updateContext context elementId = context
  { contextDepth = contextDepth context + 1
  , contextParent = Just elementId
  }
```

### 開始タグレンダリング

#### `renderOpenTag :: Text -> VProps -> Text -> RenderOptions -> State RenderEngine Text`
```haskell
renderOpenTag tag props elementId options = do
  let VProps attrs events styles className = props
  attrsHTML <- renderAttributes attrs options
  stylesHTML <- renderStylesHTML styles
  classHTML <- renderClassHTML className
  eventsHTML <- if enableEventHandlers options 
                then renderEventsHTML events elementId
                else return ""
  dataAttrsHTML <- if includeDataAttributes options
                   then return $ " data-vdom-id=\"" <> escapeHTML elementId <> "\""
                   else return ""
  
  let allAttrs = T.concat [attrsHTML, stylesHTML, classHTML, eventsHTML, dataAttrsHTML]
  return $ "<" <> tag <> allAttrs <> ">"
```

設定に応じて適切な属性を持つ開始タグを生成。

### 属性レンダリング

#### 基本属性
```haskell
renderAttributes :: Map Text Text -> RenderOptions -> State RenderEngine Text
renderAttributes attrs options = 
  return $ Map.foldlWithKey (\acc k v -> acc <> " " <> k <> "=\"" <> escapeHTML v <> "\"") "" attrs
```

#### スタイル属性
```haskell
renderStylesHTML :: Map Text Text -> State RenderEngine Text
renderStylesHTML styles = 
  if Map.null styles
    then return ""
    else do
      let styleText = Map.foldlWithKey (\acc k v -> 
            if T.null acc then k <> ": " <> v else acc <> "; " <> k <> ": " <> v) "" styles
      return $ " style=\"" <> escapeHTML styleText <> "\""
```

#### イベント属性
```haskell
renderEventsHTML :: Map Text EventHandler -> Text -> State RenderEngine Text
renderEventsHTML events elementId = do
  let eventAttrs = Map.foldlWithKey (\acc eventType _ -> 
        acc <> " data-event-" <> eventType <> "=\"" <> elementId <> "\"") "" events
  engine <- get
  let newEvents = Map.foldlWithKey (\acc eventType' (EventHandler eType _) -> 
        EventBinding elementId eventType' eType : acc) [] events
  return eventAttrs
```

## DOM操作生成

### `generateDOMOperations :: VNode -> [DOMOperation]`
```haskell
generateDOMOperations vnode = execWriter (generateOpsForNode vnode Nothing)

generateOpsForNode :: VNode -> Maybe Text -> Writer [DOMOperation] Text
generateOpsForNode (VText text) parentId = do
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
  
  unless (Map.null styles) $
    mapM_ (\(prop, val) -> tell [SetProperty nodeId ("style." <> prop) val]) (Map.toList styles)
  
  case className of
    Just cls -> tell [SetAttribute nodeId "class" cls]
    Nothing -> return ()
  
  mapM_ (\(eventType, _) -> tell [SetEventListener nodeId eventType nodeId]) (Map.toList events)
  
  case parentId of
    Just pid -> tell [AppendChild pid nodeId]
    Nothing -> return ()
  
  mapM_ (\child -> generateOpsForNode child (Just nodeId)) children
  
  return nodeId
```

クライアントサイドでのDOM操作命令を生成。SPA機能の基盤。

## 最適化機能

### `optimizeRender :: VNode -> VNode`
```haskell
optimizeRender = optimizeTextNodes . removeEmptyNodes . mergeAdjacentText
```

複数の最適化を組み合わせ。

### 空ノード除去
```haskell
removeEmptyNodes :: VNode -> VNode
removeEmptyNodes (VText text) = if T.null (T.strip text) then VText "" else VText text
removeEmptyNodes (VElement tag props children key) = 
  let filteredChildren = filter (not . isEmptyNode) (map removeEmptyNodes children)
  in VElement tag props filteredChildren key
```

### 隣接テキストノードのマージ
```haskell
mergeAdjacentText :: VNode -> VNode
mergeAdjacentText (VElement tag props children key) = 
  VElement tag props (mergeTextNodes children) key

mergeTextNodes :: [VNode] -> [VNode]
mergeTextNodes [] = []
mergeTextNodes [node] = [mergeAdjacentText node]
mergeTextNodes (VText t1 : VText t2 : rest) = mergeTextNodes (VText (t1 <> t2) : rest)
mergeTextNodes (node : rest) = mergeAdjacentText node : mergeTextNodes rest
```

### テキストノード最適化
```haskell
optimizeTextNodes :: VNode -> VNode
optimizeTextNodes (VText text) = VText (T.strip text)
optimizeTextNodes (VElement tag props children key) = 
  VElement tag props (map optimizeTextNodes children) key
```

## クライアントサイドサポート

### JavaScript生成
```haskell
generateClientScript :: [EventBinding] -> Text
generateClientScript events = 
  let eventBindings = map generateEventBinding events
  in T.concat
    [ "<script>\n"
    , "// Virtual DOM Event Bindings\n"
    , T.concat eventBindings
    , "</script>\n"
    ]

generateEventBinding :: EventBinding -> Text
generateEventBinding (EventBinding elementId eventType handler) = 
  T.concat
    [ "document.querySelector('[data-vdom-id=\""
    , elementId
    , "\"]').addEventListener('"
    , eventType
    , "', "
    , handler
    , ");\n"
    ]
```

### パフォーマンス監視
```haskell
data RenderStats = RenderStats
  { renderTime :: Double
  , nodesRendered :: Int
  , cacheHits :: Int
  , cacheMisses :: Int
  } deriving (Show, Eq)
```

### キャッシュ機能
```haskell
renderWithCache :: Map Text VNode -> VNode -> (Text, Map Text VNode)
renderWithCache cache vnode = 
  let engine = createRenderEngine defaultRenderOptions { enableCaching = True }
      engine' = engine { engineCache = cache }
      result = executeRender engine' (renderVNodeWithContext vnode)
      newCache = engineCache $ execState (renderVNodeWithContext vnode) engine'
  in (result, newCache)
```

## 重要な特徴

### 状態管理
複雑なアプリケーション状態を適切に管理。

### 設定可能性
用途に応じてレンダリング動作をカスタマイズ可能。

### 最適化
複数の最適化手法でパフォーマンスを向上。

### 拡張性
新機能を簡単に追加できるモジュラー設計。

### セキュリティ
XSS攻撃を防ぐ自動エスケープ処理。

### 将来性
SPA開発の基盤となる機能群を提供。

このレンダリングエンジンにより、Zinコンパイラは単純な静的サイト生成から複雑なWebアプリケーション開発まで対応できる強力な基盤を提供します。