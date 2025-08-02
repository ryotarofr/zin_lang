## コードブロック

コードブロックは必ず 2 行以上で構成され、2 行目は言語を指定する

```zin
cb : hoge : ["a", "b", "c"]
zin       :   hoge | i, r |
          :   Log: r
```

以下のようにコードブロック内で zin lang を書いた場合、その部分はコードブロックとして扱われるのでコンパイラは無効になります。

```zin
cb : p bold[0:5] : Hello World
zin : h1 : Documentation
    : ul : Item one
    :    : Item two
```
