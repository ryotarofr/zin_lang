# VirtualCodeGen.hs - 仮想DOMコード生成

## 概要

`VirtualCodeGen.hs`は、意味解析済みのASTから**仮想DOM**を生成するモジュールです。これはコンパイルパイプラインの重要な段階で、Zinのマークアップ構造を効率的に処理可能な仮想DOM構造に変換します。

## 主要な関数

### エントリーポイント

#### `generateVDOMFromDocument :: Document -> VDocument`
```haskell
generateVDOMFromDocument :: Document -> VDocument
generateVDOMFromDocument = generateVDOM

generateVDOM :: Document -> VDocument
generateVDOM (Document blocks) = 
  let bodyNode = createElementNode "div" [] (map blockToVNode blocks)
  in VDocument 
    { vDocumentTitle = "Zin Document"
    , vDocumentMeta = Map.fromList [("charset", "utf-8")]
    , vDocumentBody = bodyNode
    }
```

ドキュメント全体を仮想DOMに変換し、適切なメタデータとタイトルを設定します。

#### `createDocumentVDOM :: Document -> VNode`
```haskell
createDocumentVDOM (Document blocks) =
  createElementNode "html" [] 
    [ createElementNode "head" []
        [ createElementNode "meta" [Attribute "charset" "utf-8"] []
        , createElementNode "title" [] [createTextNode "Zin Document"]
        ]
    , createElementNode "body" [] (map blockToVNode blocks)
    ]
```

完全なHTML文書構造を持つ仮想DOMを生成します。

## ブロック変換

### `blockToVNode :: Block -> VNode`
各ASTブロックを対応する仮想DOMノードに変換します：

#### 段落の変換
```haskell
blockToVNode (Paragraph styles text) = 
  let styledContent = applyStylesToVNode styles text
  in createElementNode "p" [] styledContent
```

段落（`p`タグ）をスタイル適用済みの仮想DOM要素に変換。

#### 見出しの変換
```haskell
blockToVNode (Header level styles text) = 
  let headerFunc = getHeaderFunction level
      styledContent = applyStylesToVNode styles text
  in headerFunc [] styledContent

getHeaderFunction :: Int -> [Attribute] -> [VNode] -> VNode
getHeaderFunction 1 = createElementNode "h1"
getHeaderFunction 2 = createElementNode "h2"
getHeaderFunction 3 = createElementNode "h3"
getHeaderFunction 4 = createElementNode "h4"
getHeaderFunction _ = createElementNode "h1"  -- Default to h1
```

見出しレベルに応じて適切な`h1`～`h4`要素を生成。

#### リストの変換
```haskell
blockToVNode (List listType items) = 
  let listFunc = getListFunction listType
      itemNodes = map listItemToVNode items
  in listFunc (getListAttributes listType) itemNodes

getListFunction :: ListType -> [Attribute] -> [VNode] -> VNode
getListFunction Unordered = createElementNode "ul"
getListFunction Ordered = createElementNode "ol"
getListFunction Todo = createElementNode "ul"  -- Todo lists are ul with special class

getListAttributes :: ListType -> [Attribute]
getListAttributes Todo = [Attribute "class" "todo-list"]
getListAttributes _ = []
```

リストタイプに応じて`ul`または`ol`要素を生成。ToDoリストには特別なクラスを付与。

#### コードブロックの変換
```haskell
blockToVNode (CodeBlock lang code) = 
  createElementNode "pre" [] 
    [ createElementNode "code" (if T.null lang then [] else [Attribute "class" ("language-" <> lang)]) 
        [ createTextNode code ]
    ]
```

プログラムコードを`pre` + `code`要素の組み合わせで表現。言語指定があれば適切なクラスを設定。

#### 引用の変換
```haskell
blockToVNode (Quote styles text) = 
  let styledContent = applyStylesToVNode styles text
  in createElementNode "blockquote" [] styledContent
```

引用を`blockquote`要素として表現。

#### テーブルの変換
```haskell
blockToVNode (Table tables) = 
  createElementNode "table" [] (map tableRowToVNode tables)

tableRowToVNode :: Table -> VNode
tableRowToVNode (HeaderRow cells) = 
  createElementNode "tr" [] (map (\cell -> createElementNode "th" [] [createTextNode cell]) cells)
tableRowToVNode (DataRow cells) = 
  createElementNode "tr" [] (map (\cell -> createElementNode "td" [] [createTextNode cell]) cells)
```

テーブルを`table` + `tr` + `th`/`td`の標準的なHTML構造で表現。

## スタイル適用システム

### `applyStylesToVNode :: [Style] -> Text -> [VNode]`
```haskell
applyStylesToVNode [] text = [createTextNode text]
applyStylesToVNode styles text = 
  let sortedStyles = sortBy (comparing getStyleStart) styles
  in applyStylesSequentially sortedStyles text 0
```

テキストに複数のスタイルを適用します。スタイルは開始位置でソートされ、順次適用されます。

### 順次スタイル適用
```haskell
applyStylesSequentially :: [Style] -> Text -> Int -> [VNode]
applyStylesSequentially [] text offset = [createTextNode (T.drop offset text)]
applyStylesSequentially (style:restStyles) text offset = 
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
```

複雑なロジックでテキストを分割し、各部分に適切なスタイルを適用：

1. **範囲調整**: スタイル範囲をテキスト境界内に調整
2. **テキスト分割**: スタイル前・スタイル部分・スタイル後に分割
3. **再帰処理**: 残りのスタイルを後続テキストに適用

### 個別スタイル変換
```haskell
styleToVNode :: Style -> Text -> VNode
styleToVNode (Bold _) text = createElementNode "strong" [] [createTextNode text]
styleToVNode (Italic _) text = createElementNode "em" [] [createTextNode text]
styleToVNode (Strike _) text = createElementNode "del" [] [createTextNode text]
styleToVNode (Link _ url) text = createElementNode "a" [Attribute "href" url] [createTextNode text]
```

各スタイルタイプを対応するHTML要素に変換：
- **Bold** → `<strong>`
- **Italic** → `<em>`
- **Strike** → `<del>`
- **Link** → `<a href="...">`

## 高度な機能

### ネストしたスタイル処理
```haskell
applyNestedStylesToVNode :: [Style] -> Text -> [VNode]
applyNestedStylesToVNode styles text = 
  let styleRanges = map (\s -> (getStyleRange s, s)) styles
      sortedRanges = sortBy (comparing fst) styleRanges
      groupedStyles = groupNonOverlappingStyles sortedRanges
  in applyStyleGroups groupedStyles text 0
```

重複しないスタイルグループを形成し、適切にネストした構造を生成。

### CSS属性生成
```haskell
stylesToAttributes :: [Style] -> [Attribute]
stylesToAttributes styles = 
  let styleMap = Map.fromList $ concatMap styleToCSS styles
      styleAttr = if Map.null styleMap 
                  then []
                  else [Attribute "style" (renderCSSMap styleMap)]
  in styleAttr

styleToCSS :: Style -> [(Text, Text)]
styleToCSS (Bold _) = [("font-weight", "bold")]
styleToCSS (Italic _) = [("font-style", "italic")]
styleToCSS (Strike _) = [("text-decoration", "line-through")]
styleToCSS (Link _ _) = [("color", "blue"), ("text-decoration", "underline")]
```

スタイル情報をCSS属性に変換する代替アプローチ。

### メタデータ付きドキュメント生成
```haskell
generateVDOMWithMeta :: Document -> Text -> Map.Map Text Text -> VDocument
generateVDOMWithMeta doc title meta = 
  let baseDom = generateVDOM doc
  in baseDom { vDocumentTitle = title, vDocumentMeta = meta }
```

カスタムタイトルとメタデータでドキュメントを生成。

## コンポーネント指向ヘルパー

### 特化コンポーネント
```haskell
createCodeBlockComponent :: Text -> Text -> VNode
createCodeBlockComponent lang code = 
  let attrs = if T.null lang 
              then [Attribute "class" "code-block"]
              else [Attribute "class" ("code-block language-" <> lang)]
  in createElementNode "div" attrs
    [ createElementNode "pre" []
        [ createElementNode "code" [] [createTextNode code] ]
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
```

再利用可能なコンポーネント関数。特定の用途に最適化された構造を提供。

## 重要な特徴

### 型安全性
Haskellの型システムにより、不正な変換や構造を防止。

### 効率的な処理
遅延評価により、必要な部分のみを計算。

### 拡張性
新しいブロックタイプやスタイルを簡単に追加可能。

### セキュリティ
自動的なHTMLエスケープ処理でXSS攻撃を防止。

### モジュラー設計
各変換ロジックが独立しており、個別にテスト・修正が可能。

この仮想DOM生成システムにより、ZinのASTを効率的で安全なWeb表現に変換し、最終的なHTML出力の基盤を提供します。