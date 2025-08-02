# Parser.hs - 構文解析（Syntax Analysis）

## 概要

`Parser.hs`は、字句解析器から受け取ったトークン列を**抽象構文木（AST）**に変換する**構文解析器（Parser）**を実装するモジュールです。これはコンパイラの第2段階で、言語の文法規則に従ってトークンを構造化します。

## 主要なデータ型

### `Parser`型
```haskell
type Parser = ExceptT ParseError (State ParserState)
```

構文解析器の型定義：
- **`ExceptT ParseError`**: エラーハンドリングのためのモナド変換子
- **`State ParserState`**: パーサーの状態を管理
- エラーと状態の両方を組み合わせて使用

### `ParserState`
```haskell
data ParserState = ParserState
  { currentLine :: Int
  , currentColumn :: Int
  , tokens :: [Token]
  , pendingStyles :: [Style]
  , pendingURL :: Maybe Text
  } deriving (Show)
```

パーサーの内部状態：
- **`currentLine/currentColumn`**: 現在の解析位置
- **`tokens`**: 残りのトークン列
- **`pendingStyles`**: 処理中のスタイル情報
- **`pendingURL`**: 処理中のURL情報

## 主要な関数

### エントリーポイント

#### `parseZinFile :: Text -> Either ParseError Document`
```haskell
parseZinFile input = 
  let indentedLines = lexer input
      tokenList = tokenize indentedLines
  in parseDocument tokenList
```

Zinファイル全体を解析するメイン関数：
1. 字句解析を実行
2. トークン化
3. 構文解析してDocumentを生成

#### `parseDocument :: [Token] -> Either ParseError Document`
```haskell
parseDocument tokens = evalState (runExceptT parseDoc) initialState
  where
    initialState = ParserState 1 1 tokens [] Nothing
    parseDoc = do
      blocks <- parseBlocks
      return (Document blocks)
```

トークン列をDocumentに変換します。

### ブロック解析

#### `parseBlocks :: Parser [Block]`
```haskell
parseBlocks = do
  token <- peekToken
  case token of
    EOF -> return []
    Newline -> do
      _ <- consumeToken
      parseBlocks
    Comment _ -> do
      _ <- consumeToken
      parseBlocks
    Indent _ -> do
      _ <- consumeToken
      parseBlocks
    _ -> do
      block <- parseBlock
      rest <- parseBlocks
      return (block : rest)
```

複数のブロックを順次解析します。不要なトークン（改行、コメント、インデント）をスキップしながら処理を続けます。

#### `parseBlock :: Parser Block`
```haskell
parseBlock = do
  token <- peekToken
  case token of
    TopLevelTag tag -> parseTopLevelBlock tag
    _ -> throwError (UnexpectedToken token)
```

個別のブロックを解析します。トップレベルタグに基づいて適切なパーサーに分岐します。

### 要素別パーサー

#### 段落パーサー
```haskell
parseParagraph :: Parser Block
parseParagraph = do
  _ <- consumeToken -- consume 'p'
  styles <- collectStyles
  expectColon
  text <- parseTextContent
  continuationText <- parseContinuationLines
  let fullText = if T.null continuationText
                then text
                else text <> " " <> continuationText
  return (Paragraph styles fullText)
```

段落（`p`タグ）を解析：
1. `p`タグを消費
2. スタイル情報を収集
3. コロン（`:`）を期待
4. テキスト内容を解析
5. 継続行があれば連結

#### 見出しパーサー
```haskell
parseHeader :: Text -> Parser Block
parseHeader tag = do
  consumeToken -- consume header tag
  styles <- collectStyles
  expectColon
  text <- parseTextContent
  case getHeaderLevel tag of
    Just level -> return (Header level styles text)
    Nothing -> throwError (UnknownTag tag)
```

見出し（`h1`～`h4`）を解析し、レベルを適切に設定します。

#### リストパーサー
```haskell
parseList :: Text -> Parser Block
parseList tag = do
  consumeToken -- consume list tag
  case getListType tag of
    Just listType -> do
      items <- parseListItems
      return (List listType items)
    Nothing -> throwError (UnknownTag tag)

parseListItems :: Parser [ListItem]
parseListItems = do
  token <- peekToken
  case token of
    Colon -> do
      _ <- consumeToken
      text <- parseTextContent
      let item = ListItem [] text
      rest <- parseListItems
      return (item : rest)
    _ -> return []
```

リスト要素を解析。コロンから始まる各項目を順次処理します。

#### テーブルパーサー
```haskell
parseTableCells :: Parser [Text]
parseTableCells = do
  fullText <- parseTableText
  return (parseTableCellsFromText fullText)

parseTableText :: Parser Text
parseTableText = do
  token <- peekToken
  case token of
    Text content -> do
      _ <- consumeToken
      rest <- parseTableText
      return (content <> rest)
    Pipe -> do
      _ <- consumeToken
      rest <- parseTableText
      return ("|" <> rest)
    _ -> return ""
```

テーブルの特別な処理。パイプ（`|`）で区切られたセルを解析します。

### スタイル処理

#### `collectStyles :: Parser [Style]`
```haskell
collectStyles = do
  token <- peekToken
  case token of
    StyleTag name range -> do
      consumeToken
      url <- collectURL
      let style = createStyle name range url
      rest <- collectStyles
      return (style : rest)
    _ -> return []
```

スタイルタグを順次収集し、`Style`データ型に変換します。

#### `createStyle :: Text -> [Int] -> Maybe Text -> Style`
```haskell
createStyle "bold" [s, e] _ = Bold (s, e)
createStyle "italic" [s, e] _ = Italic (s, e)
createStyle "strike" [s, e] _ = Strike (s, e)
createStyle "link" [s, e] (Just url) = Link (s, e) url
createStyle "link" [s, e] Nothing = Link (s, e) ""
createStyle _ _ _ = Bold (0, 0) -- fallback
```

スタイル名と範囲から適切な`Style`コンストラクターを選択します。

### 継続行処理

#### `parseContinuationLines :: Parser Text`
```haskell
parseContinuationLines = do
  token <- peekToken
  case token of
    Newline -> do
      _ <- consumeToken
      nextToken <- peekToken
      case nextToken of
        Indent _ -> do
          _ <- consumeToken
          colonToken <- peekToken
          case colonToken of
            Colon -> do
              _ <- consumeToken
              text <- parseTextContent
              restText <- parseContinuationLines
              return $ if T.null restText
                      then text
                      else text <> " " <> restText
            _ -> return ""
        _ -> return ""
    _ -> return ""
```

複数行にわたるコンテンツを処理します。インデントとコロンで始まる行を継続行として認識します。

## ユーティリティ関数

### トークン操作
```haskell
peekToken :: Parser Token
peekToken = do
  state <- get
  case tokens state of
    [] -> return EOF
    (t:_) -> return t

consumeToken :: Parser Token
consumeToken = do
  state <- get
  case tokens state of
    [] -> return EOF
    (t:ts) -> do
      put state { tokens = ts }
      return t
```

- **`peekToken`**: 次のトークンを確認（消費しない）
- **`consumeToken`**: 次のトークンを取得して消費

### 期待値チェック
```haskell
expectColon :: Parser ()
expectColon = do
  token <- consumeToken
  case token of
    Colon -> return ()
    _ -> throwError MissingColon
```

特定のトークンが期待される位置でのチェック関数。

## エラーハンドリング

パーサーは`ExceptT ParseError`を使用して、以下のエラーを適切に処理します：

- **`UnknownTag`**: 未知のタグ
- **`MissingColon`**: 期待されるコロンの不在
- **`UnexpectedToken`**: 予期しないトークン
- **`InvalidTableFormat`**: 不正なテーブル形式

## 重要な特徴

### モナド的構成
複数のモナドを組み合わせて、エラーハンドリングと状態管理を同時に実現しています。

### 再帰的解析
多くの関数が再帰的に定義されており、ネストした構造を自然に処理できます。

### 型安全性
Haskellの型システムにより、不正な組み合わせをコンパイル時に検出できます。

この構文解析の結果として生成されたASTは、次の段階の意味解析（Semantic Analysis）で検証され、最終的にコード生成に渡されます。