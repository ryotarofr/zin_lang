使用方法

# ビルド

cabal build

# コンパイル実行

cabal run zin-compiler mock/example_simple.zin

# テスト実行

cabal test

## wasm

1. GHC WASM backend のインストール:
   curl --proto '=https' --tlsv1.2 -sSf https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/raw/master/install.sh | sh

2. ビルド:
   ./build-wasm.sh

3. テスト:
   python3 -m http.server 8000

# http://localhost:8000/example-wasm.html にアクセス

JavaScript から以下のように使用できます:

import ZinCompiler from './zin-wasm.js';

const compiler = new ZinCompiler();
await compiler.init();

const html = compiler.compile('h1 "Hello WASM!"');
console.log(html);
