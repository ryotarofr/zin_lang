# Zin
(｡ŏ﹏ŏ)

## 🏗️ アーキテクチャ

Zin コンパイラのモジュール構成：

1. **Lexer** - 字句解析（テキスト → トークン）
2. **Parser** - 構文解析（トークン → AST）
3. **Semantic** - 意味解析と妥当性検証
4. **CodeGen** - HTML 生成（直接変換）
5. **VirtualDOM** - 仮想 DOM 経由での変換
6. **Renderer** - 高度なレンダリング機能

```
zin → Lexer → Parser → Semantic → CodeGen/VirtualDOM → HTML
```

## 📚 サンプルコード

[sample](https://github.com/ryotarofr/zin_lang/blob/main/mock/example_simple.zin)

```zin
h1 bold[0:15] : Getting Started with Zin

p : Welcome to the zin markup language documentation.
  : This guide will help you understand all the features.

h2 : Text Styling Examples

p bold[0:4] italic[5:9] strike[10:16] : Bold text sample here
p link[0:9] url[https://zin-lang.org] : Visit our website for more information.

ul : Simple and intuitive syntax
   : Powerful styling capabilities
   : Multiple output formats supported

cb lang[python]:
: def fibonacci(n)
:    if n <= 1:
:      return n
:    return fibonacci(n- fibonacci(n-2))

t : | Framework | Performance | Learning Curve |
r : | React | High | Medium |
r : | Vue | High | Low |
r : | Svelte | Very High | Low |

q italic[0:8] : Remember: simplicity is the ultimate sophistication.
```
