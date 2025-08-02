## スタイルの表現方法について

### スタイルタグの種類

| 予約語 | 説明                   |
| ------ | ---------------------- |
| link   | link を適用            |
| bold   | 太字                   |
| italic | 斜体                   |
| strike | 取り消し線             |
| url    | url 必ず link に紐づく |

## 基本構文

スタイルタグはトップレベルタグに紐づく。

複数のスタイルがあれば連続して記述する

`トップレベルタグ スタイルタグ[startインデックス:endインデックス] スタイルタグ[startインデックス:endインデックス] : テキスト `

## サンプルコード

改行する場合はネストで表現する。

```zin
p
  link[0:2] url[https://~]
  bold[0:2]
  italic[4:5]
  strike[4:5]
   : Hello World
h1 : Hello World2
```

すべてのスタイルを 1 行でつなぐこともできる。

```zin
p flex link[0:2] url[https://~] bold[0:2] italic[4:5] strike[4:5] : Hello World
h1 : Hello World2
```

もしくは

```zin
p link[0:2] url[https://~] bold[0:2] italic[4:5] strike[4:5]
   : Hello World
h1 : Hello World2
```

スタイルの指定順に決まりはない。
