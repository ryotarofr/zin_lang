## system design

## 拡張子
.hmd - Hierarchical Markdown（階層的マークダウン）

## 基本形
Paragraph : 
Headding1 : 
Headding2 : 
Headding3 : 
Headding4 : 
UList     : 
OList     : 
TodoList  : 
CodeBlock : 
Link      : 
Quate     : 
Table     : |  |  |  | 
Row       : |  |  |  | 

## 省略形
P  : 
H1 : 
H2 : 
H3 : 
H4 : 
UL : 
OL : 
TL : 
CB : 
Li : 
Q  : 
T  : |  |  |  |
R  : |  |  |  |

## 詳細表示
### 基本形
Paragraph : 
  |- Link[0:2] Text[Hello] Url[https://~]
  |- Bold[0:2] Italic[4:5] Strike[4:5]

### 省略系
P : 
|- Link[0:2] Bold[0:2] Italic[4:5] Strike[4:5]
    |- Text[Hello] Url[https://~]

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

初期表示のロードで世代番号を新規発行したASTを作成

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
      TextDetail,
      TextDetail,
    ],
    children: True,
  },
  "2": {
    ...
  }
}


### children: bool の用途について
意味解析とスタイル調整を柔軟にするためのパラメータ。
textの文字列が長い場合に、IME入力の場合は「確定時」、アルファベット入力の場合はスペースが入った時に、自動で改行するべきなのかを判断する。
この時に、現在入力した内容が、前の文章と関連しているかを判定する際に用いる。
また、スタイルの調整やウィンドウサイズを変更した際に、自動で改行を入れるような制御を追加する際に利用できるはず。

つまり、childrenがTrueの場合は、parentIdの文章と関連があることを示唆する。

→ TODO(いつか考える) スタイル調整時などに自動で改行を入れる制御作れるのか


## フォーマッター
### 基本形
予約変数(11文字左詰め):空白1個, 任意文字
___________:_Hello World

## 省略形
予約変数(3文字左詰め):空白1個, 任意文字
___:_Hello World

## 詳細表示
|- 予約変数[開始位置:終了位置]
- 開始位置:終了位置 インデックス番号

Paragraph : 
  |- Link[0:2] Bold[0:2] Italic[4:5] Strike[4:5]
      |- Text[Hello] Url[https://~]

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
Text      : Hello World 
- 選択範囲が無い場合「ctrl + b」で太字モードにする
  もう一度「ctrl + b」で無効
- 選択範囲がある場合、その部分だけを太字にする

### 斜体
Text      : Hello World 
- 選択範囲が無い場合「ctrl + i」で斜体モードにする
  もう一度「ctrl + i」で無効
- 選択範囲がある場合、その部分だけを斜体にする

### 取り消し線
Text      : Hello World 
- 選択範囲が無い場合「ctrl + shift + s」取り消し線でモードにする
  もう一度「ctrl + shift + s」で無効
- 選択範囲がある場合、その部分だけを取り消し線にする

## 改行
Text      : 
          : ← 改行時はこのように : でつなぐ
          : 
          : 

テーブルの場合は Row でつなぐ
Table     : |  |  |  |
Row       : |  |  |  |


### 改行のフォーマッターの仕様1
すでに次の行に改行がある場合は改行の自動補間は無効にする

1. この状態で改行を入れる
Text      : 
          :  

2. 
Text      : 
          :  