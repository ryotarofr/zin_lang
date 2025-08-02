# VirtualDOM.hs - 仮想DOM（Virtual DOM）システム

## 概要

`VirtualDOM.hs`は、現代的なWebフレームワーク（React、Vue.jsなど）で使用されている**仮想DOM**の概念を実装するモジュールです。HTMLの実際のDOM操作を抽象化し、効率的なレンダリングと差分更新を可能にします。

## 主要なデータ型

### `VNode` - 仮想ノード
```haskell
data VNode
  = VElement 
    { vTag :: Text
    , vProps :: VProps
    , vChildren :: [VNode]
    , vKey :: Maybe Text
    }
  | VText Text
  | VComponent 
    { vComponentName :: Text
    , vComponentProps :: VProps
    , vComponentChildren :: [VNode]
    }
  deriving (Show, Eq)
```

仮想DOMの基本要素：

- **`VElement`**: HTML要素（`div`, `p`, `span`など）
  - `vTag`: タグ名
  - `vProps`: プロパティ（属性、スタイル、イベント）
  - `vChildren`: 子要素のリスト
  - `vKey`: 差分計算のためのキー（オプショナル）

- **`VText`**: テキストノード
- **`VComponent`**: カスタムコンポーネント

### `VProps` - 仮想プロパティ
```haskell
data VProps = VProps
  { vAttributes :: Map Text Text
  , vEvents :: Map Text EventHandler
  , vStyles :: Map Text Text
  , vClassName :: Maybe Text
  } deriving (Show, Eq)
```

要素のプロパティを管理：
- **`vAttributes`**: HTML属性（`id`, `href`など）
- **`vEvents`**: イベントハンドラー
- **`vStyles`**: インラインスタイル
- **`vClassName`**: CSSクラス

### `EventHandler` - イベントハンドラー
```haskell
data EventHandler = EventHandler
  { eventType :: Text
  , handler :: Text -> IO ()
  }

instance Show EventHandler where
  show (EventHandler eType _) = "EventHandler(" <> T.unpack eType <> ")"

instance Eq EventHandler where
  (EventHandler t1 _) == (EventHandler t2 _) = t1 == t2
```

イベント処理を抽象化。関数は比較できないため、イベントタイプのみで等価性を判定します。

### `VDocument` - 仮想ドキュメント
```haskell
data VDocument = VDocument
  { vDocumentTitle :: Text
  , vDocumentMeta :: Map Text Text
  , vDocumentBody :: VNode
  } deriving (Show, Eq)
```

HTML文書全体を表現：
- **`vDocumentTitle`**: ドキュメントタイトル
- **`vDocumentMeta`**: メタ情報
- **`vDocumentBody`**: ボディ要素

## 作成関数

### 基本的な作成関数
```haskell
createVNode :: Text -> VProps -> [VNode] -> VNode
createVNode tag props children = VElement tag props children Nothing

createTextNode :: Text -> VNode
createTextNode = VText

createElementNode :: Text -> [Attribute] -> [VNode] -> VNode
createElementNode tag attrs children = 
  let props = foldl addAttributeToProps emptyProps attrs
  in VElement tag props children Nothing
```

### 空のプロパティ
```haskell
emptyProps :: VProps
emptyProps = VProps Map.empty Map.empty Map.empty Nothing
```

### プロパティ操作
```haskell
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
```

## レンダリング機能

### HTML出力
```haskell
renderVNode :: VNode -> Text
renderVNode (VText text) = escapeHTML text
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
```

仮想ノードをHTMLテキストに変換します。キーがある場合は`data-key`属性を追加します。

### プロパティレンダリング
```haskell
renderProps :: VProps -> Text
renderProps (VProps attrs _ styles className) = 
  let attrText = Map.foldlWithKey renderAttribute "" attrs
      styleText = if Map.null styles then "" else " style=\"" <> renderStyles styles <> "\""
      classText = case className of
        Nothing -> ""
        Just cls -> " class=\"" <> escapeHTML cls <> "\""
  in attrText <> styleText <> classText

renderStyles :: Map Text Text -> Text
renderStyles styles = 
  T.intercalate "; " $ map (\(prop, val) -> prop <> ": " <> val) (Map.toList styles)
```

## 差分計算（Diffing）

### パッチデータ型
```haskell
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
```

仮想DOM間の違いを表現するデータ型。

### 差分計算アルゴリズム
```haskell
diffVNodes :: VNode -> VNode -> Maybe Patch
diffVNodes oldNode newNode
  | oldNode == newNode = Nothing
  | otherwise = case (oldNode, newNode) of
      (VText oldText, VText newText) -> 
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
              (Just propsPatch', children) -> Just propsPatch'
      _ -> Just (Replace newNode)
```

効率的な差分計算：
1. **同一チェック**: 完全に同じ場合は差分なし
2. **タイプチェック**: 異なるタイプは完全置換
3. **タグチェック**: 異なるタグは完全置換
4. **プロパティ比較**: プロパティの変更を検出
5. **子要素比較**: 子要素の追加・削除・更新を検出

### 子要素の差分計算
```haskell
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
```

## 便利なヘルパー関数

### HTML要素作成関数
```haskell
h1, h2, h3, h4 :: [Attribute] -> [VNode] -> VNode
p, div, span :: [Attribute] -> [VNode] -> VNode
ul, ol, li :: [Attribute] -> [VNode] -> VNode
table, tr, td, th :: [Attribute] -> [VNode] -> VNode
strong, em, del, a :: [Attribute] -> [VNode] -> VNode
blockquote, pre, code :: [Attribute] -> [VNode] -> VNode
```

よく使用されるHTML要素の作成関数群。

### セキュリティ
```haskell
escapeHTML :: Text -> Text
escapeHTML = T.replace "&" "&amp;" 
           . T.replace "<" "&lt;" 
           . T.replace ">" "&gt;" 
           . T.replace "\"" "&quot;" 
           . T.replace "'" "&#39;"
```

HTMLエスケープ処理により、XSS攻撃を防止します。

## 重要な特徴

### 不変性
すべての仮想DOM構造は不変です。変更は新しい構造を作成することで行われます。

### 効率的な差分計算
変更された部分のみを特定し、最小限のDOM操作で更新を実現します。

### 型安全性
Haskellの型システムにより、不正な構造や操作をコンパイル時に検出できます。

### 宣言的プログラミング
UIの状態を宣言的に記述でき、手続き的なDOM操作を隠蔽します。

### コンポーネント指向
再利用可能なコンポーネントを作成し、複雑なUIを構築できます。

この仮想DOMシステムにより、Zinコンパイラは効率的で安全なHTML生成を実現し、将来的にはインタラクティブなWebアプリケーションの基盤としても活用できます。