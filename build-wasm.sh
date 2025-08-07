#!/bin/bash

# Zin Compiler WASM Build Script
# このスクリプトはZinコンパイラをWebAssemblyにビルドします

set -e

echo "🔥 Zin Compiler WASM Build Script"
echo "================================="

# Check if GHC WASM backend is available
if ! command -v wasm32-wasi-ghc &> /dev/null; then
    echo "❌ Error: GHC WASM backend (wasm32-wasi-ghc) not found"
    echo "Please install GHC WASM backend from:"
    echo "https://gitlab.haskell.org/ghc/ghc-wasm-meta"
    exit 1
fi

echo "✅ GHC WASM backend found"

# Clean previous builds
echo "🧹 Cleaning previous builds..."
rm -rf dist-newstyle-wasm/
rm -f *.wasm *.js

# Configure for WASM build
echo "⚙️  Configuring for WASM build..."
cabal configure --with-ghc=wasm32-wasi-ghc --with-ghc-pkg=wasm32-wasi-ghc-pkg

# Build the WASM executable
echo "🔨 Building WASM executable..."
cabal build zin-compiler-wasm --builddir=dist-newstyle-wasm

# Find the generated WASM file
WASM_FILE=$(find dist-newstyle-wasm -name "zin-compiler-wasm" -type f | head -1)

if [ -z "$WASM_FILE" ]; then
    echo "❌ Error: WASM file not found after build"
    exit 1
fi

echo "✅ WASM file built: $WASM_FILE"

# Copy WASM file to project root
cp "$WASM_FILE" ./zin-compiler-wasm.wasm
echo "📁 WASM file copied to: ./zin-compiler-wasm.wasm"

# Generate size information
WASM_SIZE=$(stat -c%s "./zin-compiler-wasm.wasm" 2>/dev/null || stat -f%z "./zin-compiler-wasm.wasm" 2>/dev/null || echo "unknown")
echo "📊 WASM file size: $WASM_SIZE bytes"

# Check if wasm-opt is available for optimization
if command -v wasm-opt &> /dev/null; then
    echo "⚡ Optimizing WASM with wasm-opt..."
    wasm-opt -Oz --enable-bulk-memory --enable-sign-ext "./zin-compiler-wasm.wasm" -o "./zin-compiler-wasm-optimized.wasm"
    
    OPTIMIZED_SIZE=$(stat -c%s "./zin-compiler-wasm-optimized.wasm" 2>/dev/null || stat -f%z "./zin-compiler-wasm-optimized.wasm" 2>/dev/null || echo "unknown")
    echo "📊 Optimized WASM file size: $OPTIMIZED_SIZE bytes"
    
    mv "./zin-compiler-wasm-optimized.wasm" "./zin-compiler-wasm.wasm"
    echo "✅ Optimization complete"
else
    echo "⚠️  wasm-opt not found, skipping optimization"
    echo "   Install binaryen for smaller WASM files"
fi

echo ""
echo "🎉 Build complete!"
echo "Files generated:"
echo "  - zin-compiler-wasm.wasm (WebAssembly module)"
echo "  - zin-wasm.js (JavaScript interface)"
echo "  - example-wasm.html (Demo page)"
echo ""
echo "To test locally:"
echo "  1. Start a local HTTP server:"
echo "     python3 -m http.server 8000"
echo "  2. Open http://localhost:8000/example-wasm.html"
echo ""
echo "Note: WASM modules require a web server (cannot open file:// directly)"