# Semantic.hs - 意味解析（Semantic Analysis）

## 概要

`Semantic.hs`は、構文解析で生成されたASTの**意味的妥当性**を検証する**意味解析器**を実装するモジュールです。これは構文的には正しくても、論理的に問題がある構造を検出し、修正します。

## 主要なデータ型

### `SemanticAnalyzer`
```haskell
type SemanticAnalyzer = ExceptT ParseError IO
```

意味解析器の型定義。エラーハンドリング機能を持つモナド変換子です。

## 主要な関数

### エントリーポイント

#### `semanticAnalysis :: Document -> Either ParseError Document`
```haskell
semanticAnalysis doc = 
  case validateDocument doc of
    Left err -> Left err
    Right validDoc -> Right validDoc
```

ドキュメント全体の意味解析を実行する関数。検証に成功した場合は修正済みのDocumentを返します。

### ドキュメント検証

#### `validateDocument :: Document -> Either ParseError Document`
```haskell
validateDocument (Document blocks) = do
  validatedBlocks <- mapM validateBlock blocks
  return (Document validatedBlocks)
```

ドキュメント内の全ブロックを順次検証します。`mapM`により、一つでもエラーがあれば全体が失敗します。

### ブロック検証

#### `validateBlock :: Block -> Either ParseError Block`
```haskell
validateBlock block = case block of
  Paragraph styles text -> do
    validatedStyles <- validateStyles styles text
    return (Paragraph validatedStyles text)
    
  Header level styles text -> do
    when (level < 1 || level > 4) $
      Left (UnknownTag ("h" <> T.pack (show level)))
    validatedStyles <- validateStyles styles text
    return (Header level validatedStyles text)
    
  List listType items -> do
    validatedItems <- mapM validateListItem items
    return (List listType validatedItems)
    
  CodeBlock lang code -> do
    when (T.null code) $
      Left InvalidCodeBlockFormat
    return (CodeBlock lang code)
    
  Quote styles text -> do
    validatedStyles <- validateStyles styles text
    return (Quote validatedStyles text)
    
  Table tables -> do
    validateTableStructure tables
    return (Table tables)
```

各ブロックタイプに応じた検証を実行：

- **Paragraph/Quote**: スタイル情報を検証
- **Header**: レベルが1-4の範囲内であることを確認
- **List**: 各リスト項目を検証
- **CodeBlock**: コードが空でないことを確認
- **Table**: テーブル構造の整合性を確認

### スタイル検証

#### `validateStyles :: [Style] -> Text -> Either ParseError [Style]`
```haskell
validateStyles styles text = do
  mapM (validateStyle text) styles
  checkStyleOverlaps styles
  return styles
```

スタイル配列全体を検証し、重複チェックも実行します。

#### `validateStyle :: Text -> Style -> Either ParseError Style`
```haskell
validateStyle text style = do
  let (start, end) = getStyleRange style
  let textLength = T.length text
  
  when (start < 0) $
    Left StyleRangeOutOfBounds
    
  when (end > textLength) $
    Left StyleRangeOutOfBounds
    
  when (start > end) $
    Left StyleRangeOutOfBounds
    
  case style of
    Link _ url -> when (T.null url) $ Left URLNotFound
    _ -> return ()
    
  return style
```

個別のスタイルに対する検証：
1. **範囲チェック**: 開始位置が0以上
2. **範囲チェック**: 終了位置がテキスト長以下
3. **論理チェック**: 開始位置 ≤ 終了位置
4. **リンクチェック**: リンクスタイルにURLが設定されている

### スタイル重複チェック

#### `checkStyleOverlaps :: [Style] -> Either ParseError ()`
```haskell
checkStyleOverlaps styles = do
  let ranges = map getStyleRange styles
  let sortedRanges = sort ranges
  checkOverlapsInSorted sortedRanges
  return ()

checkOverlapsInSorted :: [(Int, Int)] -> Either ParseError ()
checkOverlapsInSorted [] = return ()
checkOverlapsInSorted [_] = return ()
checkOverlapsInSorted ((s1, e1) : (s2, e2) : rest) = do
  when (e1 > s2) $ 
    Left (InvalidStyleRange "Overlapping style ranges not allowed")
  checkOverlapsInSorted ((s2, e2) : rest)
```

スタイル範囲の重複を検出：
1. 範囲をソート
2. 隣接する範囲の重複をチェック
3. 重複があればエラーを報告

### テーブル構造検証

#### `validateTableStructure :: [Table] -> Either ParseError ()`
```haskell
validateTableStructure [] = return () -- 空のテーブルも許可
validateTableStructure tables = do
  let (headers, rows) = partitionTables tables
  case headers of
    [] -> return () -- ヘッダーなしも暫定的に許可
    [HeaderRow headerCols] -> do
      let expectedCols = length headerCols
      mapM_ (validateRowColumns expectedCols) rows
    _ -> return () -- 複数ヘッダーも暫定的に許可
```

テーブルの構造的整合性を確認：
1. ヘッダーとデータ行を分離
2. ヘッダーがある場合、すべての行の列数が一致するかチェック

#### `partitionTables :: [Table] -> ([Table], [Table])`
```haskell
partitionTables = foldr classify ([], [])
  where
    classify h@(HeaderRow _) (headers, rows) = (h : headers, rows)
    classify r@(DataRow _) (headers, rows) = (headers, r : rows)
```

テーブル要素をヘッダーとデータ行に分類します。

#### `validateRowColumns :: Int -> Table -> Either ParseError ()`
```haskell
validateRowColumns expected (DataRow cols) = do
  when (length cols /= expected) $ Left InconsistentTableColumns
validateRowColumns _ (HeaderRow _) = return ()
```

データ行の列数がヘッダーと一致するか確認します。

### リスト項目検証

#### `validateListItem :: ListItem -> Either ParseError ListItem`
```haskell
validateListItem (ListItem styles text) = do
  validatedStyles <- validateStyles styles text
  return (ListItem validatedStyles text)
```

リスト項目内のスタイルを検証します。

## ユーティリティ関数

### 条件チェック
```haskell
when :: Bool -> Either ParseError () -> Either ParseError ()
when True action = action
when False _ = return ()
```

条件付きでエラーチェックを実行するヘルパー関数。

### テキスト正規化
```haskell
normalizeText :: Text -> Text
normalizeText = T.strip . T.unwords . T.words

validateTextNotEmpty :: Text -> Either ParseError ()
validateTextNotEmpty text = do
  when (T.null (normalizeText text)) $ Left EmptyInput
```

テキストの正規化と空チェック。

### スタイル適用と検証
```haskell
applyValidatedStyles :: [Style] -> Text -> Either ParseError Text
applyValidatedStyles styles text = do
  validatedStyles <- validateStyles styles text
  return $ foldr applyStyleToText text validatedStyles
```

検証済みスタイルをテキストに適用します。

### 重複検出
```haskell
hasStyleConflicts :: [Style] -> Bool
hasStyleConflicts styles = 
  let ranges = map getStyleRange styles
      sortedRanges = sort ranges
  in hasOverlaps sortedRanges

hasOverlaps :: [(Int, Int)] -> Bool
hasOverlaps [] = False
hasOverlaps [_] = False
hasOverlaps ((_, e1) : (s2, _) : rest) = e1 > s2 || hasOverlaps rest
```

スタイルの競合を検出するヘルパー関数。

## 重要な特徴

### 段階的検証
意味解析は複数の段階に分かれており、各段階で特定の側面を検証します：

1. **構造的検証**: ブロックの基本構造
2. **範囲検証**: スタイル範囲の妥当性
3. **整合性検証**: テーブル列数の一致など
4. **論理検証**: 重複やリンクURLの存在など

### エラーの早期検出
問題を早期に発見することで、後続の処理でのエラーを防ぎます。

### 型安全性
Haskellの型システムにより、すべての検証ケースを漏れなく処理することが保証されます。

### 柔軟な拡張性
新しい検証ルールを追加する際も、既存のコードを変更することなく簡単に拡張できます。

この意味解析により、構文的には正しくても意味的に問題のあるコードを事前に検出し、安全なコード生成を実現しています。