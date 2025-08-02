# Main.hs - プログラムのエントリーポイント

## 概要

`Main.hs`は、このZin言語コンパイラのエントリーポイントです。コマンドライン引数を処理し、ファイルをコンパイルする流れを制御します。

## 主要な関数

### `main :: IO ()`
プログラムの開始点です。以下の処理を行います：

1. **コマンドライン引数の取得**: `getArgs`を使用
2. **引数の解析と処理**:
   - 引数なし: 使用方法を表示してエラー終了
   - `--help`: ヘルプメッセージを表示して正常終了
   - 1つの引数: 入力ファイル名（出力は`.html`拡張子で自動生成）
   - 2つの引数: 入力ファイル名と出力ファイル名を指定
   - 3つ以上: エラー終了

### `compileFile :: String -> String -> IO ()`
実際のコンパイル処理を行う関数です：

```haskell
compileFile inputPath outputPath = do
  fileExists <- doesFileExist inputPath
  when (not fileExists) $ do
    putStrLn $ "Error: File not found: " ++ inputPath
    exitFailure
    
  putStrLn $ "Compiling " ++ inputPath ++ " -> " ++ outputPath
  
  input <- TIO.readFile inputPath
  case compileZin input of
    Left err -> do
      let errorInfo = createError (T.pack inputPath) 1 1 err
      TIO.putStrLn (formatError errorInfo)
      exitFailure
    Right html -> do
      TIO.writeFile outputPath html
      putStrLn $ "Successfully compiled to " ++ outputPath
      exitSuccess
```

### `compileZin :: T.Text -> Either ParseError T.Text`
Zinファイルのコンパイルパイプラインです：

1. **構文解析** (`parseZinFile`): Zinファイルをパースして抽象構文木（AST）を生成
2. **意味解析** (`semanticAnalysis`): ASTの妥当性をチェック
3. **仮想DOM生成** (`generateVDOMFromDocument`): ASTから仮想DOMを生成
4. **HTML出力** (`renderToHTML`): 仮想DOMからHTMLを生成

## 重要なポイント

### エラーハンドリング
- `Either`型を使用して、エラーと成功の両方のケースを安全に処理
- エラーが発生した場合は、詳細なエラーメッセージを表示

### テキスト処理
- `Data.Text`を使用して効率的なテキスト処理を実現
- UTF-8対応

### ファイル操作
- 入力ファイルの存在確認
- 安全なファイル読み書き

## コンパイルパイプライン

```
Zinファイル → Lexer → Parser → Semantic → VirtualDOM → Renderer → HTML
```

1. **字句解析**: テキストをトークンに分解
2. **構文解析**: トークンからAST（抽象構文木）を構築
3. **意味解析**: ASTの妥当性をチェック
4. **仮想DOM生成**: ASTから仮想DOMを生成
5. **レンダリング**: 仮想DOMからHTMLを出力

このパイプラインにより、Zin言語のマークアップをHTMLに変換します。