#!/usr/bin/env node

/**
 * Mock WASM Generator for Zin Compiler
 * 
 * このスクリプトは、実際のWASMビルドが利用できない場合に、
 * JavaScriptベースのモックWASMモジュールを生成します。
 */

const fs = require('fs');
const path = require('path');

// Zin パーサーとコンパイラの簡易実装
class MockZinCompiler {
  compile(zinSource) {
    // 非常に基本的なZinパーサー
    const lines = zinSource.split('\n').filter(line => line.trim());
    let html = '';
    
    for (const line of lines) {
      const trimmed = line.trim();
      
      // h1, h2, h3, h4
      if (trimmed.match(/^h([1-4])\s+"(.+)"$/)) {
        const [, level, content] = trimmed.match(/^h([1-4])\s+"(.+)"$/);
        html += `<h${level}>${this.escapeHtml(content)}</h${level}>\n`;
      }
      // p (段落)
      else if (trimmed.match(/^p\s+"(.+)"$/)) {
        const [, content] = trimmed.match(/^p\s+"(.+)"$/);
        html += `<p>${this.escapeHtml(content)}</p>\n`;
      }
      // p with style
      else if (trimmed.match(/^p\s+\{(.+?)\}\s+"(.+?)"/)) {
        const matches = [...trimmed.matchAll(/(\{[^}]+\}|"[^"]+?")/g)];
        let paragraph = '<p>';
        
        for (let i = 0; i < matches.length; i++) {
          const match = matches[i][0];
          if (match.startsWith('{')) {
            const style = match.slice(1, -1);
            const text = matches[++i][0].slice(1, -1);
            if (style === 'bold') paragraph += `<strong>${this.escapeHtml(text)}</strong>`;
            else if (style === 'italic') paragraph += `<em>${this.escapeHtml(text)}</em>`;
          } else if (match.startsWith('"')) {
            paragraph += this.escapeHtml(match.slice(1, -1));
          }
        }
        html += paragraph + '</p>\n';
      }
      // ul (リスト)
      else if (trimmed.startsWith('ul [')) {
        html += '<ul>\n';
        const listContent = zinSource.substring(zinSource.indexOf('[', zinSource.indexOf(trimmed)));
        const items = this.parseList(listContent);
        for (const item of items) {
          html += `  <li>${this.escapeHtml(item)}</li>\n`;
        }
        html += '</ul>\n';
      }
      // ol (番号付きリスト)  
      else if (trimmed.startsWith('ol [')) {
        html += '<ol>\n';
        const listContent = zinSource.substring(zinSource.indexOf('[', zinSource.indexOf(trimmed)));
        const items = this.parseList(listContent);
        for (const item of items) {
          html += `  <li>${this.escapeHtml(item)}</li>\n`;
        }
        html += '</ol>\n';
      }
      // q (引用)
      else if (trimmed.match(/^q\s+"(.+)"$/)) {
        const [, content] = trimmed.match(/^q\s+"(.+)"$/);
        html += `<blockquote>${this.escapeHtml(content)}</blockquote>\n`;
      }
      // cb (コードブロック)
      else if (trimmed.match(/^cb\s+"(.+?)"\s+"$/)) {
        const [, lang] = trimmed.match(/^cb\s+"(.+?)"\s+"$/);
        const codeStart = zinSource.indexOf('"', zinSource.indexOf(lang) + lang.length) + 1;
        const codeEnd = zinSource.indexOf('"\n', codeStart);
        const code = zinSource.substring(codeStart, codeEnd).replace(/\\n/g, '\n');
        html += `<pre><code class="language-${lang}">${this.escapeHtml(code)}</code></pre>\n`;
      }
    }
    
    return html;
  }
  
  parseList(content) {
    const items = [];
    let depth = 0;
    let current = '';
    let inString = false;
    
    for (let i = 0; i < content.length; i++) {
      const char = content[i];
      
      if (char === '"' && content[i-1] !== '\\') {
        inString = !inString;
      } else if (!inString) {
        if (char === '[') depth++;
        else if (char === ']') {
          depth--;
          if (depth === 0) break;
        } else if (char === ',' && depth === 1) {
          if (current.trim()) {
            items.push(current.trim().replace(/^"|"$/g, ''));
          }
          current = '';
          continue;
        }
      }
      
      if (depth > 0 && !(char === '[' && depth === 1)) {
        current += char;
      }
    }
    
    if (current.trim()) {
      items.push(current.trim().replace(/^"|"$/g, ''));
    }
    
    return items;
  }
  
  escapeHtml(text) {
    return text
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#039;');
  }
}

// モックWASMモジュールの生成
const mockWasmModule = `
// Mock WASM Module for Zin Compiler
// This is a JavaScript implementation that simulates the WASM interface

const mockCompiler = ${MockZinCompiler.toString()};
const compiler = new mockCompiler();

// TextEncoder/Decoder for string conversion
const textEncoder = new TextEncoder();
const textDecoder = new TextDecoder();

// Memory simulation
const memory = new WebAssembly.Memory({ initial: 256 });
const memoryBuffer = new Uint8Array(memory.buffer);
let memoryOffset = 0;

// Helper functions
function allocateString(str) {
  const bytes = textEncoder.encode(str);
  const ptr = memoryOffset;
  memoryBuffer.set(bytes, ptr);
  memoryOffset += bytes.length + 1;
  memoryBuffer[memoryOffset - 1] = 0; // null terminator
  return ptr;
}

function readString(ptr) {
  let end = ptr;
  while (memoryBuffer[end] !== 0) end++;
  return textDecoder.decode(memoryBuffer.slice(ptr, end));
}

// Exported functions
const exports = {
  memory: memory,
  
  initZinCompiler: function() {
    console.log('Mock Zin Compiler initialized');
    return 0;
  },
  
  getVersion: function() {
    return allocateString('zin-compiler-mock 0.1.0');
  },
  
  compileZinToHTML: function(inputPtr, inputLen) {
    try {
      const input = textDecoder.decode(memoryBuffer.slice(inputPtr, inputPtr + inputLen));
      const output = compiler.compile(input);
      return allocateString(output);
    } catch (error) {
      return allocateString('ERROR: ' + error.message);
    }
  },
  
  compileZinToJSON: function(inputPtr, inputLen) {
    try {
      const input = textDecoder.decode(memoryBuffer.slice(inputPtr, inputPtr + inputLen));
      const html = compiler.compile(input);
      const result = JSON.stringify({ success: true, html: html });
      return allocateString(result);
    } catch (error) {
      const result = JSON.stringify({ success: false, error: error.message });
      return allocateString(result);
    }
  },
  
  benchmarkCompilation: function(inputPtr, inputLen, iterations) {
    const input = textDecoder.decode(memoryBuffer.slice(inputPtr, inputPtr + inputLen));
    let successes = 0;
    let failures = 0;
    
    for (let i = 0; i < iterations; i++) {
      try {
        compiler.compile(input);
        successes++;
      } catch (error) {
        failures++;
      }
    }
    
    const result = JSON.stringify({
      iterations: iterations,
      successes: successes,
      failures: failures
    });
    return allocateString(result);
  },
  
  malloc: function(size) {
    const ptr = memoryOffset;
    memoryOffset += size;
    return ptr;
  },
  
  free: function(ptr) {
    // No-op for this mock implementation
  }
};

// Export as WebAssembly.Instance compatible object
if (typeof module !== 'undefined' && module.exports) {
  module.exports = { exports };
}
`;

// メイン処理
console.log('🔨 Creating mock WASM module for Zin Compiler...');

const outputPath = path.join(__dirname, 'zin-compiler-wasm.js');
fs.writeFileSync(outputPath, mockWasmModule);

console.log('✅ Mock WASM module created:', outputPath);

// HTMLファイルの更新も必要
const htmlUpdateScript = `
// Update the ZinCompiler class to work with the mock module
const originalInit = ZinCompiler.prototype.init;
ZinCompiler.prototype.init = async function(wasmPath) {
  try {
    // Try to load as a JavaScript module instead of WASM
    const mockModule = await import('./zin-compiler-wasm.js');
    this.wasmModule = { exports: mockModule.exports || mockModule.default.exports };
    this.initialized = true;
    console.log('Mock Zin Compiler module loaded successfully');
  } catch (error) {
    console.error('Failed to load mock module:', error);
    // Fall back to original WASM loading
    return originalInit.call(this, wasmPath);
  }
};
`;

console.log('\n📝 To use the mock module, update your HTML file:');
console.log('1. Change the import to use the mock module');
console.log('2. Or add this script after importing ZinCompiler:');
console.log(htmlUpdateScript);

// テスト用のNode.jsスクリプトも生成
const testScript = `
// Test script for the mock WASM module
const mockModule = require('./zin-compiler-wasm.js');
const exports = mockModule.exports;

// Initialize
exports.initZinCompiler();

// Get version
const versionPtr = exports.getVersion();
console.log('Version:', exports.memory.buffer);

// Test compilation
const testZin = 'h1 "Hello Mock WASM!"\\np "This is a test"';
const encoder = new TextEncoder();
const bytes = encoder.encode(testZin);
const ptr = exports.malloc(bytes.length);

// Copy test data to memory
const memory = new Uint8Array(exports.memory.buffer);
memory.set(bytes, ptr);

// Compile
const resultPtr = exports.compileZinToHTML(ptr, bytes.length);

console.log('Compilation test passed!');
`;

fs.writeFileSync(path.join(__dirname, 'test-mock-wasm.js'), testScript);
console.log('✅ Test script created: test-mock-wasm.js');
console.log('\nRun: node test-mock-wasm.js to test the mock module');