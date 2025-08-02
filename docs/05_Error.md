# Error.hs - エラーハンドリングシステム

## 概要

`Error.hs`は、Zinコンパイラ全体で使用される**エラーハンドリングシステム**を提供するモジュールです。構文解析エラーから実行時エラーまで、一貫したエラー報告を行います。

## 主要なデータ型

### `ParseError`
```haskell
data ParseError
  = UnknownTag Text
  | InvalidStyleRange Text
  | MissingColon
  | InvalidTableFormat
  | UnexpectedToken Token
  | IndentationError
  | URLNotFound
  | StyleRangeOutOfBounds
  | EmptyInput
  | InvalidCodeBlockFormat
  | MissingTableHeader
  | InconsistentTableColumns
  deriving (Show, Eq)
```

Zinコンパイラで発生する可能性のあるエラー種別：

- **`UnknownTag`**: 未知のタグ（例：`xyz`タグ）
- **`InvalidStyleRange`**: 不正なスタイル範囲指定
- **`MissingColon`**: 期待されるコロン（`:`）の不在
- **`InvalidTableFormat`**: テーブル形式エラー
- **`UnexpectedToken`**: 予期しないトークン
- **`IndentationError`**: インデンテーションエラー
- **`URLNotFound`**: リンクスタイルのURL不在
- **`StyleRangeOutOfBounds`**: スタイル範囲がテキスト範囲外
- **`EmptyInput`**: 空入力
- **`InvalidCodeBlockFormat`**: コードブロック形式エラー
- **`MissingTableHeader`**: テーブルヘッダー不在
- **`InconsistentTableColumns`**: テーブル列数不整合

### `ErrorInfo`
```haskell
data ErrorInfo = ErrorInfo
  { fileName :: Text
  , lineNumber :: Int
  , columnNumber :: Int
  , errorType :: ParseError
  , suggestion :: Maybe Text
  } deriving (Show, Eq)
```

エラーの詳細情報：
- **`fileName`**: エラーが発生したファイル名
- **`lineNumber`**: 行番号
- **`columnNumber`**: 列番号
- **`errorType`**: エラーの種類
- **`suggestion`**: 修正提案（オプショナル）

## 主要な関数

### エラーフォーマット

#### `formatError :: ErrorInfo -> Text`
```haskell
formatError err = T.concat
  [ fileName err
  , ":"
  , T.pack (show (lineNumber err))
  , ":"
  , T.pack (show (columnNumber err))
  , ": error: "
  , errorMessage (errorType err)
  , case suggestion err of
      Just sug -> "\n  suggestion: " <> sug
      Nothing -> ""
  ]
```

エラー情報を読みやすい形式でフォーマットします。出力例：
```
document.zin:3:5: error: Missing colon ':' after tag
  suggestion: Add ':' after the tag, e.g., 'p : text'
```

#### `errorMessage :: ParseError -> Text`
```haskell
errorMessage (UnknownTag tag) = "Unknown tag '" <> tag <> "'"
errorMessage (InvalidStyleRange range) = "Invalid style range '" <> range <> "'"
errorMessage MissingColon = "Missing colon ':' after tag"
errorMessage InvalidTableFormat = "Invalid table format"
errorMessage (UnexpectedToken token) = "Unexpected token: " <> T.pack (show token)
-- ... 他のエラーメッセージ
```

各エラータイプに対応する人間が読みやすいメッセージを生成します。

### 修正提案システム

#### `renderErrorWithSuggestion :: ParseError -> Maybe Text`
```haskell
renderErrorWithSuggestion (UnknownTag tag) = 
  Just $ "Use one of: p, h1, h2, h3, h4, ul, ol, tl, cb, q, t, r instead of '" <> tag <> "'"
renderErrorWithSuggestion (InvalidStyleRange _) = 
  Just "Use format: bold[0:5] or italic[2:8]"
renderErrorWithSuggestion MissingColon = 
  Just "Add ':' after the tag, e.g., 'p : text'"
renderErrorWithSuggestion InvalidTableFormat = 
  Just "Use format: t : | col1 | col2 | col3 |"
-- ... 他の修正提案
```

各エラーに対して具体的な修正方法を提案します。これにより、ユーザーは問題を素早く解決できます。

### エラー情報作成

#### `createError :: Text -> Int -> Int -> ParseError -> ErrorInfo`
```haskell
createError file line col err = ErrorInfo
  { fileName = file
  , lineNumber = line
  , columnNumber = col
  , errorType = err
  , suggestion = renderErrorWithSuggestion err
  }
```

エラー情報を簡単に作成するためのヘルパー関数。自動的に修正提案も含めます。

## エラーメッセージの例

### 未知のタグエラー
```
document.zin:2:1: error: Unknown tag 'xyz'
  suggestion: Use one of: p, h1, h2, h3, h4, ul, ol, tl, cb, q, t, r instead of 'xyz'
```

### スタイル範囲エラー
```
document.zin:1:15: error: Invalid style range '0-5'
  suggestion: Use format: bold[0:5] or italic[2:8]
```

### コロン不在エラー
```
document.zin:3:3: error: Missing colon ':' after tag
  suggestion: Add ':' after the tag, e.g., 'p : text'
```

### テーブル形式エラー
```
document.zin:5:1: error: Invalid table format
  suggestion: Use format: t : | col1 | col2 | col3 |
```

## 重要な特徴

### 一貫性のあるエラー報告
すべてのエラーが同じ形式で報告されるため、ユーザーは一貫した体験を得られます。

### 位置情報の正確性
ファイル名、行番号、列番号を含む詳細な位置情報により、エラーの場所を特定しやすくなっています。

### 建設的な修正提案
単にエラーを報告するだけでなく、具体的な修正方法を提案することで、開発者の生産性を向上させます。

### 型安全性
Haskellの型システムにより、エラー処理の漏れを防ぎ、すべてのエラーケースが適切に処理されることを保証します。

### 拡張性
新しいエラータイプを追加する際も、既存のコードを変更することなく簡単に拡張できます。

このエラーハンドリングシステムにより、Zinコンパイラは開発者にとって使いやすく、デバッグしやすいツールとなっています。