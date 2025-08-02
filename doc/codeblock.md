## コードブロック

コードブロックの構文が更新されました。新しい構文では `cb lang[言語名]` の形式で言語を指定できます。

### 新しい構文

```zin
cb lang[python] : def greet():
                :     print("Hello World")

cb lang[javascript] : function add(a, b) { return a + b; }

cb : プレーンテキスト（言語指定なし、デフォルトでtxt形式）
```

### レガシー構文（旧形式）

```zin
cb:
  python : def greet():
       :     print("Hello World")
```

言語の設定がない場合は、デフォルトで `txt` 形式になります。
