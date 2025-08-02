# AST.hs - 抽象構文木（Abstract Syntax Tree）

## 概要

`AST.hs`は、Zin言語の抽象構文木（AST）を定義するモジュールです。ASTは、ソースコードの構造を表現するデータ構造で、コンパイラの核となる部分です。

## 主要なデータ型

### `Document`
```haskell
data Document = Document [Block]
  deriving (Show, Eq)
```
ドキュメント全体を表す最上位のデータ型。複数の`Block`で構成されます。

### `Block`
```haskell
data Block
  = Paragraph [Style] Text
  | Header Int [Style] Text
  | List ListType [ListItem]
  | CodeBlock Text Text
  | Quote [Style] Text
  | Table [Table]
  deriving (Show, Eq)
```

Zin言語の各ブロック要素を表すADT（代数データ型）です：

- **`Paragraph`**: 段落（`p`タグ）
- **`Header`**: 見出し（`h1`～`h4`タグ）
- **`List`**: リスト（`ul`, `ol`, `tl`タグ）
- **`CodeBlock`**: コードブロック（`cb`タグ）
- **`Quote`**: 引用（`q`タグ）
- **`Table`**: テーブル（`t`, `r`タグ）

### `Style`
```haskell
data Style
  = Bold (Int, Int)
  | Italic (Int, Int)
  | Strike (Int, Int)
  | Link (Int, Int) Text
  deriving (Show, Eq)
```

テキストのスタイリング情報を表します：
- 各スタイルは範囲（開始位置, 終了位置）を持ちます
- **`Bold`**: 太字スタイル
- **`Italic`**: 斜体スタイル
- **`Strike`**: 取り消し線スタイル
- **`Link`**: リンク（URLも含む）

### `ListType`
```haskell
data ListType 
  = Unordered 
  | Ordered 
  | Todo
  deriving (Show, Eq)
```

リストの種類を表します：
- **`Unordered`**: 番号なしリスト（`ul`）
- **`Ordered`**: 番号付きリスト（`ol`）
- **`Todo`**: ToDoリスト（`tl`）

### `ListItem`
```haskell
data ListItem = ListItem [Style] Text
  deriving (Show, Eq)
```

リストの各項目を表現します。スタイルとテキストを含みます。

### `Table`
```haskell
data Table 
  = HeaderRow [Text] 
  | DataRow [Text]
  deriving (Show, Eq)
```

テーブルの行を表現します：
- **`HeaderRow`**: ヘッダー行（`t`タグ）
- **`DataRow`**: データ行（`r`タグ）

## ヘルパー関数

### タグ識別関数
```haskell
getHeaderLevel :: Text -> Maybe Int
getListType :: Text -> Maybe ListType
isTopLevelTag :: Text -> Bool
isHeaderTag :: Text -> Bool
isListTag :: Text -> Bool
isTableTag :: Text -> Bool
```

これらの関数は、テキストからタグの種類や属性を判定するために使用されます。

### スタイル処理関数
```haskell
validateStyleRange :: Style -> Text -> Bool
getStyleRange :: Style -> (Int, Int)
applyStyleToText :: Style -> Text -> Text
```

- **`validateStyleRange`**: スタイルの範囲がテキストの長さ内に収まっているかチェック
- **`getStyleRange`**: スタイルの範囲（開始, 終了）を取得
- **`applyStyleToText`**: スタイルをテキストに適用してHTMLタグを挿入

## 重要なポイント

### 型安全性
Haskellの型システムを活用して、不正な組み合わせを**コンパイル時**に検出できます。例えば、`Header`に不正なレベル（5以上）を指定しようとすると型エラーになります。

### パターンマッチング
各データ型は**パターンマッチング**で処理されます。これにより、すべてのケースを漏れなく処理することが保証されます。

### 不変性
すべてのデータ構造は**不変（immutable）**です。一度作成されたASTは変更されず、新しいASTが生成されます。

## 使用例

```haskell
-- 段落の例
paragraph = Paragraph [Bold (0, 4)] "Bold text here"

-- 見出しの例  
header = Header 1 [Italic (5, 10)] "Title with italic"

-- リストの例
todoList = List Todo [
  ListItem [] "First task",
  ListItem [Strike (0, 4)] "Done task"
]
```

このASTにより、Zin言語の構造化された表現が可能になり、後続の処理（意味解析、コード生成）で安全に操作できます。