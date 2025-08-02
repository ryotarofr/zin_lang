# CodeGen.hs - HTMLコード生成

## 概要

`CodeGen.hs`は、ASTから直接HTMLコードを生成する**コード生成器**を実装するモジュールです。仮想DOMを経由しない直接的なHTML生成により、シンプルで高速な変換を実現します。

## 主要な関数

### エントリーポイント

#### `generateHTMLFromDocument :: Document -> Text`
```haskell
generateHTMLFromDocument :: Document -> Text
generateHTMLFromDocument = generateHTML

generateHTML :: Document -> Text
generateHTML (Document blocks) = 
  "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n<title>Zin Document</title>\n</head>\n<body>\n" 
  <> T.concat (map blockToHTML blocks) 
  <> "</body>\n</html>"
```

ドキュメント全体を完全なHTML文書として出力します。自動的にDOCTYPE宣言、HTMLヘッダー、メタデータを含めます。

## ブロック変換

### `blockToHTML :: Block -> Text`
各ASTブロックを対応するHTMLテキストに直接変換します：

#### 段落の変換
```haskell
blockToHTML (Paragraph styles text) = 
  let styledText = applyStylesToText styles text
  in "<p>" <> escapeHTML styledText <> "</p>\n"
```

段落を`<p>`タグで囲み、スタイルを適用したテキストを挿入。

#### 見出しの変換
```haskell
blockToHTML (Header level styles text) = 
  let tag = "h" <> T.pack (show level)
      styledText = applyStylesToText styles text
  in "<" <> tag <> ">" <> escapeHTML styledText <> "</" <> tag <> ">\n"
```

レベルに応じて`h1`～`h4`タグを動的に生成。

#### リストの変換
```haskell
blockToHTML (List listType items) = 
  let tag = getListTag listType
      itemsHTML = T.concat (map listItemToHTML items)
  in "<" <> tag <> ">\n" <> itemsHTML <> "</" <> tag <> ">\n"

getListTag :: ListType -> Text
getListTag Unordered = "ul"
getListTag Ordered = "ol"
getListTag Todo = "ul class=\"todo-list\""

listItemToHTML :: ListItem -> Text
listItemToHTML (ListItem styles text) = 
  let styledText = applyStylesToText styles text
  in "<li>" <> escapeHTML styledText <> "</li>\n"
```

リストタイプに応じて適切なHTMLタグを選択。ToDoリストには特別なクラスを付与。

#### コードブロックの変換
```haskell
blockToHTML (CodeBlock lang code) = 
  "<pre><code class=\"language-" <> escapeHTML lang <> "\">" 
  <> escapeHTML code <> "</code></pre>\n"
```

プログラムコードを`<pre><code>`の組み合わせで表現。言語情報をクラス属性に設定。

#### 引用の変換
```haskell
blockToHTML (Quote styles text) = 
  let styledText = applyStylesToText styles text
  in "<blockquote>" <> escapeHTML styledText <> "</blockquote>\n"
```

引用を`<blockquote>`タグで表現。

#### テーブルの変換
```haskell
blockToHTML (Table tables) = 
  "<table>\n" <> T.concat (map tableRowToHTML tables) <> "</table>\n"

tableRowToHTML :: Table -> Text
tableRowToHTML (HeaderRow cells) = 
  "<tr>\n" <> T.concat (map (\cell -> "<th>" <> escapeHTML cell <> "</th>\n") cells) <> "</tr>\n"
tableRowToHTML (DataRow cells) = 
  "<tr>\n" <> T.concat (map (\cell -> "<td>" <> escapeHTML cell <> "</td>\n") cells) <> "</tr>\n"
```

テーブルを標準的なHTML構造（`table` > `tr` > `th`/`td`）で出力。

## スタイル処理システム

### `applyStylesToText :: [Style] -> Text -> Text`
```haskell
applyStylesToText styles text = 
  let sortedStyles = sortBy (comparing getStyleStart) styles
  in foldr (applyStyleSafely text) text sortedStyles
```

複数のスタイルをテキストに安全に適用。スタイルは開始位置でソートされ、`foldr`で順次適用されます。

### 安全なスタイル適用
```haskell
applyStyleSafely :: Text -> Style -> Text -> Text
applyStyleSafely originalText style currentText = 
  let textLength = T.length originalText
      (start, end) = getStyleRange style
  in if start >= 0 && end <= textLength && start <= end
     then applyStyleToTextAt style start end currentText
     else currentText
```

範囲チェックを行い、無効な範囲のスタイルは無視します。これにより、部分的な破損があっても処理を継続できます。

### 位置指定スタイル適用
```haskell
applyStyleToTextAt :: Style -> Int -> Int -> Text -> Text
applyStyleToTextAt style start end text = 
  let (before, rest) = T.splitAt start text
      (middle, after) = T.splitAt (end - start) rest
      styledMiddle = case style of
        Bold _ -> "<strong>" <> middle <> "</strong>"
        Italic _ -> "<em>" <> middle <> "</em>"
        Strike _ -> "<del>" <> middle <> "</del>"
        Link _ url -> "<a href=\"" <> escapeHTML url <> "\">" <> middle <> "</a>"
  in before <> styledMiddle <> after
```

指定された位置範囲のテキストに対して、スタイルタイプに応じたHTMLタグを挿入：

- **Bold** → `<strong>...</strong>`
- **Italic** → `<em>...</em>`
- **Strike** → `<del>...</del>`
- **Link** → `<a href="...">...</a>`

### 代替スタイル適用関数
```haskell
styleToHTML :: Style -> Text -> Text
styleToHTML (Bold (s, e)) text = insertTag "strong" s e text
styleToHTML (Italic (s, e)) text = insertTag "em" s e text  
styleToHTML (Strike (s, e)) text = insertTag "del" s e text
styleToHTML (Link (s, e) url) text = insertTag ("a href=\"" <> escapeHTML url <> "\"") s e text

insertTag :: Text -> Int -> Int -> Text -> Text
insertTag tagName start end text = 
  let (before, rest) = T.splitAt start text
      (middle, after) = T.splitAt (end - start) rest
  in before <> "<" <> tagName <> ">" <> middle <> "</" <> getTagName tagName <> ">" <> after

getTagName :: Text -> Text
getTagName tagWithAttrs = T.takeWhile (/= ' ') tagWithAttrs
```

より直接的なタグ挿入アプローチ。属性付きタグの処理も可能。

## 高度なスタイル処理

### ネストしたスタイル処理
```haskell
applyNestedStyles :: [Style] -> Text -> Text
applyNestedStyles styles text = 
  let sortedStyles = sortBy (comparing getStyleRange) styles
      groupedStyles = groupNonOverlapping sortedStyles
  in foldr applyStyleGroup text groupedStyles

groupNonOverlapping :: [Style] -> [[Style]]
groupNonOverlapping [] = []
groupNonOverlapping [s] = [[s]]
groupNonOverlapping (s1:s2:rest) = 
  let (start1, end1) = getStyleRange s1
      (start2, _) = getStyleRange s2
  in if end1 <= start2
     then [s1] : groupNonOverlapping (s2:rest)
     else case groupNonOverlapping (s2:rest) of
       [] -> [[s1]]
       (group:groups) -> (s1:group) : groups
```

重複しないスタイルグループを形成し、適切にネストした構造を生成。

### スタイルグループ適用
```haskell
applyStyleGroup :: [Style] -> Text -> Text
applyStyleGroup styles text = foldr applyStyleToTextAt' text styles
  where
    applyStyleToTextAt' style txt = 
      let (start, end) = getStyleRange style
      in applyStyleToTextAt style start end txt
```

グループ内のスタイルを順次適用。

## ユーティリティ関数

### セキュリティ
```haskell
escapeHTML :: Text -> Text
escapeHTML = T.replace "&" "&amp;" 
           . T.replace "<" "&lt;" 
           . T.replace ">" "&gt;" 
           . T.replace "\"" "&quot;" 
           . T.replace "'" "&#39;"
```

XSS攻撃を防ぐためのHTMLエスケープ処理。

### ヘルパー関数
```haskell
getStyleStart :: Style -> Int
getStyleStart style = fst (getStyleRange style)

generateCodeBlock :: Text -> Text -> Text
generateCodeBlock language code = 
  "<pre><code class=\"language-" <> escapeHTML language <> "\">" 
  <> escapeHTML code <> "</code></pre>"

generateTodoListHTML :: [ListItem] -> Text
generateTodoListHTML items = 
  "<ul class=\"todo-list\">\n" 
  <> T.concat (map todoItemToHTML items) 
  <> "</ul>\n"
  where
    todoItemToHTML (ListItem styles text) = 
      let styledText = applyStylesToText styles text
      in "<li class=\"todo-item\">" <> escapeHTML styledText <> "</li>\n"
```

特定用途向けの便利な関数群。

## 重要な特徴

### 直接生成
仮想DOMを経由せず、ASTから直接HTMLを生成するため、高速で軽量。

### エラー耐性
不正なスタイル範囲や部分的な破損があっても、可能な限り処理を継続。

### セキュリティ
自動的なHTMLエスケープにより、XSS攻撃を防止。

### 読みやすい出力
生成されるHTMLは適切にフォーマットされ、デバッグしやすい。

### 拡張性
新しいブロックタイプやスタイルを簡単に追加可能。

### 型安全性
Haskellの型システムにより、不正な変換を防止。

このコード生成システムにより、ZinのASTを効率的で安全なHTMLに変換し、最終的なWeb出力を実現します。仮想DOMと並行して提供することで、用途に応じた最適な変換方法を選択できます。