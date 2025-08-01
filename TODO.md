- [] Rust で初期コンパイラを作る
- [] 初期コンパイラを使って、セルフコンパイラを作る
- [] 以降の描画処理を作る(仮想DOM → 実DOM生成 → ブラウザ描画 → ユーザー操作 → 状態更新 → 差分計算 → パッチ適用)

## 言語名
zin

## 拡張子

.zin

# 言語仕様について

## 予約語一覧

| 予約語 | 説明 |
| ---- | ---- |
| Paragraph | paragraph描画に使用 |
| Headding1 | h1描画に使用 |
| Headding2 | h2描画に使用 |
| Headding3 | h3描画に使用 |
| Headding4 | h4描画に使用 |
| UList | リスト描画に使用 |
| OList | ナンバーリスト描画に使用 |
| TodoList | todoリスト描画に使用 |
| CodeBlock | コードブロック描画に使用 |
| Quate | quate描画に使用 |
| Table | テーブルヘッダ描画に使用 |
| Row | テーブルボディ描画に使用 |
| P | Paragraphの省略形 |
| H1 | Headding1の省略形 |
| H2 | Headding2の省略形 |
| H3 | Headding3の省略形 |
| H4 | Headding4の省略形 |
| UL | UListの省略形 |
| OL | OListの省略形 |
| TL | TodoListの省略形 |
| CB | CodeBlockの省略形 |
| Q | Quateの省略形 |
| T | Tableの省略形 |
| R | Rowの省略形 |
| | |

## 基本形・省略形のモード切替
`setting.zin`ファイルに設定を記載
```zin
// 基本形
MODE = default

// or

// 省略形
MODE = omit
```

## PrimaryNode

### 基本形
```zin
ZIN : title
Paragraph : text
Headding1 : text
Headding2 : text
Headding3 : text
Headding4 : text
UList     : text
OList     : text
TodoList  : text
CodeBlock : text
Quate     : text
Table     : | text | text | text |
Row       : | text | text | text |
```

### 省略形

```zin
ZIN : title
P : text
H1 : text
H2 : text
H3 : text
H4 : text
UL : text
OL : text
TL : text
CB : text
Q : text
T : | text | text | text |
R : | text | text | text |
```

Example

```zin
Paragraph : "Hello World"
P : "Hello World"
```

### 詳細
1. 
zin : title
ここからが描画部分のコードとなる。
その時の識別子として「HMDX :」を使う。

2. 
text の部分はすべて文字列として扱う。

## SecondaryNode

PrimaryNode とセットで使う。

Link[startIndex:endIndex]
Bold[startIndex:endIndex]
Italic[startIndex:endIndex]
Strike[startIndex:endIndex]

Example

```zin
Paragraph : "Hello World"
  Bold[0:4]
  Italic[6:10]

P : "Hello World"
  Bold[0:4]
  Italic[6:10]
```

## TertiaryNode

SecondaryNode とセットで使う。

- Url はリンクにのみ紐づく
  Url[text]

Example

```zin
Paragraph : "Hello World"
  Link[0:4] Url[https://~]

Paragraph : "Hello World"
  Link[0:4]
    Url[https://~]

P : "Hello World"
  Link[0:4] Url[https://~]

P : "Hello World"
  Link[0:4]
    Url[https://~]
```


# 変数
1. 変数、関数定義位置
「Return : title」の上で定義する必要がある

2. 予約文字
PrimaryNode に定義したものと以下の変数
```zin
// ログ出力用
Log : 
```

3. 代入
型が異なる値であっても同じ変数に再代入できる
```zin
hoge: 1
hoge: "aiueo"
Log: ログ | hoge |　// aiueo
huga: タイトル

Return : | huga |
// ...
```

4. 変数のライフタイム
ファイル内で借用が無くなるまで


## 関数

基本は一番下の変数や式が戻り値となる

```zin
hoge |arg1, arg2|:
  hoge2: 1
  hoge3 : arg1 + arg2 + hoge2
  hoge3

// もしくは
hoge |arg1, arg2|:
  hoge2: 1
  arg1 + arg2 + hoge2
```

「Return」を明示的に書くこともできる

```zin
hoge |arg1, arg2|:
  hoge2: 1
  hoge3 : arg1 + arg2 + hoge2
  hoge3

// もしくは
hoge |arg1, arg2|:
  hoge2: 1
  Return arg1 + arg2 + hoge2
```

## データ型
```zin
// int
hoge : 1
// str
hoge : "hello"
// bool
hoge : True
// float
hoge : 1.1
// list
hoge : ["a", "b", "c"]
// dict
hoge : {key1: "hello", key2: 2}
```

## データ操作
```zin
// if
hoge > 1 : trueの時
         : falseの時

hoge > 1  : trueの時
hoge < 10 : else ifの条件の時

// ネストしたifはインデントを下げる
hoge > 1 : trueの時
  hoge > 2 : trueの時
           : falseの時
: falseの時


// for
hoge : ["a", "b", "c"]
hoge | i, r | 
  Log: r

```


## サンプルコード
```zin

return | title: "zin 言語の使い方" |
h1 : 実装例
p : 以下に実装例を記載
h2 : 目次
ul     : データ型の使い方
       : 関数の使い方
h2 : データ型の使い方
CodeBlock : hoge : ["a", "b", "c"]
zin       :   hoge | i, r | 
          :   Log: r
```

# AST

```rust
// 行スタイル(マークダウン踏襲)
enum LineType {
  Paragraph,
  Headding1,
  Headding2,
  Headding3,
  Headding4,
  UList,
  OList,
  TodoList,
  CodeBlock,
  Link,
  Quate,
  Table,
  Row,
}

// 標準的なスタイル(マークダウン踏襲)
enum TextType {
  Link,
  Bold,
  Italic,
  Strike,
}


// TODO もうちょっといい方法ありそう
enum LetterSpacing {
  xs: "0.025em",
  md: "0.05em",
  lg: "0.1em",
  xl: "0.15em",
  2xl:"0.2em",
  3xl:"0.25em",
  4xl:"0.3em",
  custom: usize,
}

// 文字単位のスタイル(TODO 初期値 md や primary などで設定)
struct TextStyle {
  letterSpacing: LetterSpacing, // 文字間隔
  // ...
}

struct TextDetail {
  textType: TextType,
  start: isize,
  end: isize,
  someValue: Option<usize>, // 今のところリンクの設定で使う
  style: TextStyle // より詳細なスタイルの設定
}

// 行のスタイル
enum LayoutStyle {
  Start,
  Center,
  End,
}
```

初期表示のロードで世代番号を新規発行した AST を作成

```
flatTree: {
  "1"(世代番号):{
    parentId: None,
    text: "Hello World",
    lineNumber: 1
    lineType: LineType.Paragraph,
    layoutStyle: LayoutStyle.Start,
    textDetail: [
      {
        textType: TextType.Link,
        start: 0,
        end: 4,
        someValue:"https://~",
        style: Vec![TextStyle.letterSpacing.md]
      },
      {
        textType: TextType.Bold,
        start: 6,
        end: 10,
        someValue: None,
      },
    ],
    children: True,
  },
  "2": {
    ...
  }
}
```

### children: bool の用途について

意味解析とスタイル調整を柔軟にするためのパラメータ。
text の文字列が長い場合に、IME 入力の場合は「確定時」、アルファベット入力の場合はスペースが入った時に、自動で改行するべきなのかを判断する。
この時に、現在入力した内容が、前の文章と関連しているかを判定する際に用いる。
また、スタイルの調整やウィンドウサイズを変更した際に、自動で改行を入れるような制御を追加する際に利用できるはず。

つまり、children が True の場合は、parentId の文章と関連があることを示唆する。

→ TODO(いつか考える) スタイル調整時などに自動で改行を入れる制御作れるのか

## フォーマッター

### 基本形

予約変数(11 文字左詰め):空白 1 個, 任意文字 \***\*\_\_\_\*\***:\_"Hello World"

## 省略形

予約変数(3 文字左詰め):空白 1 個, 任意文字
\_\_\_:\_"Hello World"

## 詳細表示

|- 予約変数[開始位置:終了位置]

- 開始位置:終了位置 インデックス番号

Paragraph 
|- Link[0:2] Bold[0:2] Italic[4:5] Strike[4:5]
|- Text[Hello] Url[https://~]
          : Hello World

表示優先順
Link >> Bold >> Italic >> Stlike

### オートコンプリートでサジェストを表示

- P → Paragraph
- H → Headding1~4
- U → UList
- O → OList
- L → NormalList, NumberList, TodoList
- R → Row
- C → CodeBlock
- Q → Quate

## ハイライト

### 太字

Text : "Hello World"

- 選択範囲が無い場合「ctrl + b」で太字モードにする
  もう一度「ctrl + b」で無効
- 選択範囲がある場合、その部分だけを太字にする

### 斜体

Text : "Hello World"

- 選択範囲が無い場合「ctrl + i」で斜体モードにする
  もう一度「ctrl + i」で無効
- 選択範囲がある場合、その部分だけを斜体にする

### 取り消し線

Text : "Hello World"

- 選択範囲が無い場合「ctrl + shift + s」取り消し線でモードにする
  もう一度「ctrl + shift + s」で無効
- 選択範囲がある場合、その部分だけを取り消し線にする

## 改行

```zin
Text :
     : ← 改行時はこのように : でつなぐ
     :
     :
```

テーブルの場合は Row でつなぐ
Table : | | | |
Row : | | | |

### 改行のフォーマッターの仕様 1

すでに次の行に改行がある場合は改行の自動補間は無効にする

1. この状態で改行を入れる
```zin
Text :
     :
```

2. 
```zin
Text :
______
     :

```

## コンパイラのロジック

文字が入力されたら発火

変数定義時のコンパイラ
文字が入力されて

変数に紐づく入力であるか
Yes: 型推論のコンパイラを実行

