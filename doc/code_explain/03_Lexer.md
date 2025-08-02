# Lexer.hs - 字句解析（Lexical Analysis）

## 概要

`Lexer.hs`は、Zin言語の**字句解析器（Lexer）**を実装するモジュールです。字句解析は、入力テキストを**トークン**に分解する処理で、コンパイラの最初のステップです。

## 主要なデータ型

### `Token`
```haskell
data Token
  = TopLevelTag Text
  | StyleTag Text [Int]
  | Colon
  | Pipe
  | Text Text
  | Newline
  | Indent Int
  | URL Text
  | Comment Text
  | EOF
  deriving (Show, Eq)
```

Zin言語の各要素を表すトークン：

- **`TopLevelTag`**: ブロックレベルタグ（`p`, `h1`, `ul`など）
- **`StyleTag`**: スタイルタグ（`bold[0:5]`, `italic[2:8]`など）
- **`Colon`**: コロン（`:`）
- **`Pipe`**: パイプ（`|`） - テーブル区切り文字
- **`Text`**: 通常のテキスト
- **`Newline`**: 改行
- **`Indent`**: インデント（レベル付き）
- **`URL`**: URL（`url[https://example.com]`）
- **`Comment`**: コメント（`//`で始まる行）
- **`EOF`**: ファイル終端

### `IndentedLine`
```haskell
data IndentedLine = IndentedLine
  { indentLevel :: Int
  , lineNumber :: Int
  , content :: Text
  } deriving (Show, Eq)
```

各行のインデント情報を保持するデータ型：
- **`indentLevel`**: インデントの深さ
- **`lineNumber`**: 行番号
- **`content`**: 行の内容（インデントを除く）

## 主要な関数

### `lexer :: Text -> [IndentedLine]`
```haskell
lexer input = zipWith processLine [1..] (T.lines input)
  where
    processLine lineNum line = IndentedLine
      { indentLevel = countIndent line
      , lineNumber = lineNum
      , content = T.stripStart line
      }
```

入力テキストを行単位で処理し、各行のインデント情報を抽出します。

### `tokenize :: [IndentedLine] -> [Token]`
```haskell
tokenize inputLines = concatMap tokenizeLine inputLines ++ [EOF]
  where
    tokenizeLine line
      | T.null (content line) = [Newline]
      | T.isPrefixOf "//" (content line) = [Comment (T.drop 2 (content line)), Newline]
      | otherwise = Indent (indentLevel line) : tokenizeContent (content line) ++ [Newline]
```

各行を個別のトークンに分解します：
1. 空行の場合：`Newline`トークンのみ
2. コメント行の場合：`Comment`トークンと`Newline`
3. 通常の行：`Indent` + コンテンツのトークン群 + `Newline`

### トークン解析の詳細

#### `nextToken :: Text -> LexerState (Token, Text)`
状態を管理しながら、次のトークンを解析します：

```haskell
nextToken input
  | T.null input = return (EOF, "")
  | T.head input == ':' = return (Colon, T.tail input)
  | T.head input == '|' = return (Pipe, T.tail input)
  | T.head input == ' ' = nextToken (T.dropWhile (== ' ') input)
  | isTopLevelTag input = parseTopLevelTag input
  | isStyleTag input = parseStyleTag input
  | isURL input = parseURL input
  | otherwise = parseText input
```

#### タグ認識
```haskell
isTopLevelTag :: Text -> Bool
isTopLevelTag input = any (`T.isPrefixOf` input) topLevelTags
  where
    topLevelTags = ["p", "h1", "h2", "h3", "h4", "ul", "ol", "tl", "cb", "q", "t", "r"]

isStyleTag :: Text -> Bool
isStyleTag input = any (`T.isPrefixOf` input) styleTags
  where
    styleTags = ["bold[", "italic[", "strike[", "link["]
```

## 特別な処理

### スタイルタグの解析
```haskell
parseStyleTag :: Text -> LexerState (Token, Text)
parseStyleTag input = do
  let tagName = T.takeWhile (/= '[') input
  let afterTag = T.drop (T.length tagName + 1) input
  let rangeStr = T.takeWhile (/= ']') afterTag
  let rest = T.drop (T.length rangeStr + 1) afterTag
  case parseRange rangeStr of
    Just range -> return (StyleTag tagName range, rest)
    Nothing -> parseText input
```

`bold[0:5]`のような形式を解析して、タグ名（`bold`）と範囲（`[0, 5]`）を抽出します。

### 範囲の解析
```haskell
parseRange :: Text -> Maybe [Int]
parseRange rangeStr = 
  case T.splitOn ":" rangeStr of
    [start, end] -> do
      s <- readMaybeInt start
      e <- readMaybeInt end
      return [s, e]
    _ -> Nothing
```

`0:5`の形式を`[0, 5]`の整数リストに変換します。

## 状態管理

### `LexerState`
```haskell
type LexerState = State (Int, [Token])
```

字句解析中の状態を管理するために`State`モナドを使用しています。これにより、解析位置や蓄積されたトークンを追跡できます。

## 重要な特徴

### エラー処理
- 不正な形式のスタイルタグは、通常のテキストとして処理されます
- 部分的な解析失敗でも処理を継続できます

### インデント処理
```haskell
countIndent :: Text -> Int
countIndent text = T.length $ T.takeWhile isSpaceOrTab text
  where
    isSpaceOrTab c = c == ' ' || c == '\t'
```

スペースとタブの両方をインデントとして認識します。

### 効率的なテキスト処理
- `Data.Text`を使用して高速なテキスト操作を実現
- 遅延評価により、必要な部分のみを処理

## 使用例

入力：
```
p: Hello bold[0:5] world
ul:
  : First item
  : Second item
```

出力トークン：
```
[Indent 0, TopLevelTag "p", Colon, Text "Hello ", StyleTag "bold" [0,5], Text " world", Newline,
 Indent 0, TopLevelTag "ul", Colon, Newline,
 Indent 2, Colon, Text "First item", Newline,
 Indent 2, Colon, Text "Second item", Newline,
 EOF]
```

この字句解析の結果を、次の段階のParser（構文解析器）が処理してASTを構築します。