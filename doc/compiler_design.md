# zin コンパイラ 詳細設計書

## 1. アーキテクチャ概要

zin コンパイラは以下のフェーズで構成される：

1. **字句解析** (Lexical Analysis) - テキストをトークンに分割
2. **構文解析** (Syntax Analysis) - トークンから AST を構築
3. **意味解析** (Semantic Analysis) - AST の検証とスタイル適用
4. **コード生成** (Code Generation) - HTML または他の形式に出力

## 2. トークン定義

```haskell
data Token
  = TopLevelTag String      -- p, h1, h2, ul, ol, tl, cb, q, t, r
  | StyleTag String [Int]   -- bold[0:2], link[0:5], italic[1:3], strike[2:4]
  | Colon                   -- :
  | Pipe                    -- |
  | Text String            -- 任意のテキスト
  | Newline
  | Indent Int             -- インデント数（スペース数）
  | URL String             -- url[https://example.com]
  | Comment String         -- // コメント
  | EOF                    -- ファイル終端
```

## 3. 抽象構文木（AST）定義

```haskell
data Document = Document [Block]

data Block
  = Paragraph [Style] String
  | Header Int [Style] String      -- レベル(1-4), スタイル, テキスト
  | List ListType [ListItem]
  | CodeBlock String String        -- 言語, コード内容
  | Quote [Style] String
  | Table [Table]

data Style
  = Bold (Int, Int)               -- 開始位置, 終了位置
  | Italic (Int, Int)
  | Strike (Int, Int)
  | Link (Int, Int) String        -- 範囲, リンク先URL

data ListType = Unordered | Ordered | Todo

data ListItem = ListItem [Style] String

data Table = HeaderRow [String] | DataRow [String]
```

## 4. エラーハンドリング

```haskell
data ParseError
  = UnknownTag String              -- 未知のトップレベルタグ
  | InvalidStyleRange String       -- 無効な範囲指定
  | MissingColon                   -- コロンが見つからない
  | InvalidTableFormat             -- テーブル形式が不正
  | UnexpectedToken Token          -- 予期しないトークン
  | IndentationError              -- インデントエラー
  | URLNotFound                   -- URLが指定されていない
  | StyleRangeOutOfBounds         -- スタイル範囲がテキスト範囲外
```

## 5. 字句解析仕様

### 5.1 トークン優先順位

1. コメント (`//` で始まる行)
2. トップレベルタグ（予約語）
3. スタイルタグ（`[数字:数字]`付き）
4. URL（`url[...]`形式）
5. 特殊文字（`:`, `|`）
6. テキスト

### 5.2 インデント処理

- スペース数でインデントレベルを判定
- タブは 4 スペース相当として扱う
- インデント不整合はエラーとする

```haskell
data IndentedLine = IndentedLine
  { indentLevel :: Int
  , lineNumber :: Int
  , content :: String
  }
```

## 6. 構文解析仕様

### 6.1 基本構文規則

```ebnf
document      ::= block*
block         ::= paragraph | header | list | codeblock | quote | table
paragraph     ::= "p" style* ":" text
header        ::= ("h1" | "h2" | "h3" | "h4") style* ":" text
list          ::= list_type list_item+
codeblock     ::= "cb" ":" text newline language ":" code_content
quote         ::= "q" style* ":" text
table         ::= table_header table_row*
table_header  ::= "t" ":" "|" text ("|" text)* "|"
table_row     ::= "r" ":" "|" text ("|" text)* "|"
style         ::= style_tag "[" number ":" number "]" url?
url           ::= "url" "[" uri "]"
```

### 6.2 ネスト構造の処理

```haskell
parseNested :: [IndentedLine] -> Either ParseError [Block]
```

- 同一インデントレベルで継続する場合はタグ省略可能
- スタイル指定は複数行に分けて記述可能

## 7. 意味解析仕様

### 7.1 バリデーション規則

- スタイル範囲がテキスト長を超えないことを確認
- link スタイルには url が必須
- テーブル行の列数が一致することを確認
- 未知のタグの検出

### 7.2 スタイル適用

```haskell
applyStyles :: [Style] -> String -> Either ParseError String
```

## 8. 出力生成仕様

### 8.1 HTML 出力

```haskell
-- 基本タグマッピング
tagMapping :: String -> String
tagMapping "p"  = "p"
tagMapping "h1" = "h1"
tagMapping "h2" = "h2"
tagMapping "h3" = "h3"
tagMapping "h4" = "h4"
tagMapping "ul" = "ul"
tagMapping "ol" = "ol"
tagMapping "tl" = "ul class=\"todo-list\""
tagMapping "cb" = "pre"
tagMapping "q"  = "blockquote"

-- スタイル変換
styleToHTML :: Style -> String -> String
styleToHTML (Bold (s,e)) text = insertTag "strong" s e text
styleToHTML (Italic (s,e)) text = insertTag "em" s e text
styleToHTML (Strike (s,e)) text = insertTag "del" s e text
styleToHTML (Link (s,e) url) text = insertTag ("a href=\"" ++ url ++ "\"") s e text
```

### 8.2 コードブロック処理

```haskell
generateCodeBlock :: String -> String -> String
generateCodeBlock language code =
  "<pre><code class=\"language-" ++ language ++ "\">" ++ escapeHTML code ++ "</code></pre>"
```

## 9. 設定ファイル仕様

### 9.1 setting.zin 形式

```zin
// モード設定
MODE = default  // or omit

// カスタムタグ定義（将来拡張）
CUSTOM_TAG = mytag -> div class="custom"

// 出力形式
OUTPUT = html   // or markdown
```

### 9.2 設定データ型

```haskell
data Config = Config
  { mode :: Mode
  , customTags :: [(String, String)]
  , outputFormat :: OutputFormat
  , indentSize :: Int
  }

data Mode = Default | Omit
data OutputFormat = HTML | Markdown

parseConfig :: String -> Either ParseError Config
```

## 10. エラーメッセージ仕様

エラーメッセージには以下の情報を含める：

- ファイル名
- 行番号
- 列番号
- エラーの種類
- 修正提案

```haskell
data ErrorInfo = ErrorInfo
  { fileName :: String
  , lineNumber :: Int
  , columnNumber :: Int
  , errorType :: ParseError
  , suggestion :: Maybe String
  }
```

## 11. パフォーマンス要件

- ファイルサイズ: 最大 10MB
- 処理時間: 1 秒以内（通常の文書）
- メモリ使用量: 入力サイズの 3 倍以下

## 12. 将来の拡張性

### 12.1 プラグインシステム

- カスタムタグの追加
- 独自スタイルの定義
- 出力形式の拡張

### 12.2 追加予定機能

- 数式サポート（LaTeX 記法）
- 図表の埋め込み
- 相互参照機能
