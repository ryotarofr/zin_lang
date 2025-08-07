#!/usr/bin/env node

/**
 * Updated Mock WASM Generator for Zin Compiler
 * 現在のZin構文と修正されたパーサーロジックに対応
 */

const fs = require('fs');
const path = require('path');

// Updated Zin Compiler with fixed table parsing
class UpdatedMockZinCompiler {
  compile(zinSource) {
    const lines = zinSource.split('\n');
    let html = '';
    let i = 0;
    
    while (i < lines.length) {
      const line = lines[i].trim();
      
      if (!line || line.startsWith('//')) {
        i++;
        continue;
      }
      
      // Headers (h1, h2, h3, h4)
      if (line.match(/^h[1-4]\s/)) {
        const [tag, ...rest] = line.split(' ');
        const level = tag[1];
        const content = this.parseTextWithStyles(rest.join(' '));
        html += `<h${level}>${content}</h${level}>\n`;
        i++;
      }
      // Paragraph
      else if (line.match(/^p\s/)) {
        const content = this.parseTextWithStyles(line.substring(2));
        html += `<p>${content}</p>\n`;
        i++;
      }
      // Unordered list
      else if (line.match(/^ul\s/)) {
        html += '<ul>\n';
        i++;
        while (i < lines.length && lines[i].trim().startsWith(':')) {
          const itemContent = this.parseTextWithStyles(lines[i].trim().substring(1).trim());
          html += `  <li>${itemContent}</li>\n`;
          i++;
        }
        html += '</ul>\n';
      }
      // Code block
      else if (line.match(/^cb\s/)) {
        const langMatch = line.match(/lang\[([^\]]+)\]/);
        const lang = langMatch ? langMatch[1] : 'text';
        
        html += `<pre><code class="language-${lang}">`;
        i++;
        
        // Read code block content
        while (i < lines.length && lines[i].trim().startsWith(':')) {
          const codeLine = lines[i].trim().substring(1).trim();
          html += this.escapeHtml(codeLine) + '\n';
          i++;
        }
        
        html += '</code></pre>\n';
      }
      // Table and Quote - FIXED ORDER HANDLING
      else if (line.match(/^[tr]\s/)) {
        const tableResult = this.parseTable(lines, i);
        html += tableResult.html;
        i = tableResult.nextIndex;
      }
      // Quote
      else if (line.match(/^q\s/)) {
        const content = this.parseTextWithStyles(line.substring(2));
        html += `<blockquote>${content}</blockquote>\n`;
        i++;
      }
      else {
        i++;
      }
    }
    
    return html;
  }
  
  parseTable(lines, startIndex) {
    let html = '<table>\n';
    let i = startIndex;
    
    // Continue parsing table rows until we hit a non-table line
    while (i < lines.length) {
      const line = lines[i].trim();
      
      if (line.match(/^t\s/)) {
        // Table header
        const cells = this.parseTableCells(line.substring(2));
        html += '  <tr>\n';
        for (const cell of cells) {
          html += `    <th>${this.escapeHtml(cell)}</th>\n`;
        }
        html += '  </tr>\n';
        i++;
      }
      else if (line.match(/^r\s/)) {
        // Table row
        const cells = this.parseTableCells(line.substring(2));
        html += '  <tr>\n';
        for (const cell of cells) {
          html += `    <td>${this.escapeHtml(cell)}</td>\n`;
        }
        html += '  </tr>\n';
        i++;
      }
      else if (!line) {
        // Skip empty lines within table
        i++;
      }
      else {
        // End of table
        break;
      }
    }
    
    html += '</table>\n';
    return { html, nextIndex: i };
  }
  
  parseTableCells(cellContent) {
    if (!cellContent.startsWith(':')) return [];
    
    const content = cellContent.substring(1).trim();
    const cells = content.split('|').map(cell => cell.trim()).filter(cell => cell);
    return cells;
  }
  
  parseTextWithStyles(text) {
    if (!text.startsWith(':')) return '';
    
    let content = text.substring(1).trim();
    
    // Apply styles like italic[0:8]
    const stylePattern = /(\w+)\[(\d+):(\d+)\]/g;
    let match;
    const styles = [];
    
    while ((match = stylePattern.exec(text)) !== null) {
      styles.push({
        type: match[1],
        start: parseInt(match[2]),
        end: parseInt(match[3])
      });
    }
    
    // Remove style declarations from content
    content = content.replace(/\w+\[\d+:\d+\]/g, '').trim();
    
    // Escape HTML entities before applying styles
    content = this.escapeHtml(content);
    
    // Apply styles in reverse order to handle nested styles
    styles.reverse().forEach(style => {
      const before = content.substring(0, style.start);
      const styled = content.substring(style.start, style.end);
      const after = content.substring(style.end);
      
      let styledText;
      switch (style.type) {
        case 'bold':
          styledText = `<strong>${styled}</strong>`;
          break;
        case 'italic':
          styledText = `<em>${styled}</em>`;
          break;
        case 'strike':
          styledText = `<del>${styled}</del>`;
          break;
        default:
          styledText = styled;
      }
      
      content = before + styledText + after;
    });
    
    return content;
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
// Updated Mock WASM Module for Zin Compiler
const mockCompiler = ${UpdatedMockZinCompiler.toString()};
const compiler = new mockCompiler();

const textEncoder = new TextEncoder();
const textDecoder = new TextDecoder();

const memory = new WebAssembly.Memory({ initial: 256 });
const memoryBuffer = new Uint8Array(memory.buffer);
let memoryOffset = 0;

function allocateString(str) {
  const bytes = textEncoder.encode(str);
  const ptr = memoryOffset;
  memoryBuffer.set(bytes, ptr);
  memoryOffset += bytes.length + 1;
  memoryBuffer[memoryOffset - 1] = 0;
  return ptr;
}

function readString(ptr) {
  let end = ptr;
  while (memoryBuffer[end] !== 0) end++;
  return textDecoder.decode(memoryBuffer.slice(ptr, end));
}

const exports = {
  memory: memory,
  
  initZinCompiler: function() {
    console.log('Updated Mock Zin Compiler initialized');
    return 0;
  },
  
  getVersion: function() {
    return allocateString('zin-compiler-updated-mock 0.1.0');
  },
  
  compileZinToHTML: function(inputPtr, inputLen) {
    try {
      const input = textDecoder.decode(memoryBuffer.slice(inputPtr, inputPtr + inputLen));
      const output = compiler.compile(input);
      return allocateString(output);
    } catch (error) {
      console.error('Compilation error:', error);
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
      console.error('JSON compilation error:', error);
      const result = JSON.stringify({ success: false, error: error.message });
      return allocateString(result);
    }
  },
  
  benchmarkCompilation: function(inputPtr, inputLen, iterations) {
    const input = textDecoder.decode(memoryBuffer.slice(inputPtr, inputPtr + inputLen));
    let successes = 0;
    let failures = 0;
    const startTime = performance.now();
    
    for (let i = 0; i < iterations; i++) {
      try {
        compiler.compile(input);
        successes++;
      } catch (error) {
        failures++;
      }
    }
    
    const endTime = performance.now();
    const totalTimeMs = endTime - startTime;
    
    const result = JSON.stringify({
      iterations: iterations,
      successes: successes,
      failures: failures,
      totalTimeMs: totalTimeMs,
      avgTimePerIterationMs: totalTimeMs / iterations
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

if (typeof module !== 'undefined' && module.exports) {
  module.exports = { exports };
}
`;

// ファイル出力
console.log('🔨 Creating updated mock WASM module for Zin Compiler...');

const outputPath = path.join(__dirname, 'zin-compiler-wasm.js');
fs.writeFileSync(outputPath, mockWasmModule);

console.log('✅ Updated mock WASM module created:', outputPath);

// テスト用スクリプト
const testScript = `
const mockModule = require('./zin-compiler-wasm.js');
const exports = mockModule.exports;

exports.initZinCompiler();

const testZin = \`t : | head | head | head |
r : | row | row | row |
r : | row | row | row |
r : | row | row | row |

q italic[0:8] : Remember: simplicity is the ultimate sophistication.\`;

const encoder = new TextEncoder();
const bytes = encoder.encode(testZin);
const ptr = exports.malloc(bytes.length);

const memory = new Uint8Array(exports.memory.buffer);
memory.set(bytes, ptr);

const resultPtr = exports.compileZinToJSON(ptr, bytes.length);
const result = memory.slice(resultPtr);
const resultStr = new TextDecoder().decode(result.slice(0, result.indexOf(0)));

console.log('Test result:');
console.log(resultStr);
`;

fs.writeFileSync(path.join(__dirname, 'test-updated-mock-wasm.js'), testScript);
console.log('✅ Test script created: test-updated-mock-wasm.js');