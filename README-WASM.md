# Zin Compiler WebAssembly版

このプロジェクトは、HaskellのZinコンパイラをWebAssembly経由でブラウザから利用できるようにしたものです。

## 現在の実装状況

### ✅ 実装済み機能

1. **基本的な要素**
   - 見出し (h1, h2, h3, h4)
   - 段落 (p)
   - 引用 (q)
   - リスト (ul, ol, tl)

2. **テキストスタイル**
   - 太字: `bold[開始:終了]`
   - 斜体: `italic[開始:終了]`
   - 取り消し線: `strike[開始:終了]`
   - リンク: `link[開始:終了] url[URL]`

3. **新しい構文形式**
   ```
   要素 スタイル属性 : コンテンツ
   ```
   例:
   ```
   h1 bold[0:15] : Getting Started with Zin
   p bold[0:4] italic[5:9] : Bold text sample here
   ```

### 🚧 未実装機能
- テーブル (t, r)
- コードブロック (cb)
- 増分コンパイル
- ファイルI/O

## ファイル構成

- `zin-compiler-wasm.js` - JavaScript実装のモックWASMモジュール
- `zin-wasm.js` - WASMモジュールのJavaScriptインターフェース
- `example-wasm.html` - ブラウザで動作するデモページ
- `src/Zin/WASM.hs` - 将来の実際のWASMビルド用Haskellモジュール

## 使用方法

1. HTTPサーバーを起動:
   ```bash
   python3 -m http.server 8000
   ```

2. ブラウザで開く:
   ```
   http://localhost:8000/example-wasm.html
   ```

3. 左側のテキストエリアにZinコードを入力すると、自動的にHTMLに変換されます。

## 技術的な詳細

### モックWASMモジュール
現在の実装は、実際のWASMではなくJavaScriptでWASM APIをシミュレートしています。これは：
- GHC WASMバックエンドがまだ実験的なため
- JavaScriptFFIのサポートに制限があるため

### メモリ管理
WebAssembly.Memoryを使用してWASMのメモリモデルを模倣し、文字列の受け渡しを行っています。

## 今後の計画

1. **実際のWASMビルド**
   - GHC WASMバックエンドの安定版を待つ
   - またはAsteriusなどの代替コンパイラを検討

2. **機能追加**
   - テーブルとコードブロックのサポート
   - より複雑な構文のサポート
   - パフォーマンス最適化

3. **開発者向け機能**
   - エラーメッセージの改善
   - デバッグモードの追加
   - パーサーのテストスイート