// Updated Mock WASM Module for Zin Compiler
const mockCompiler = class UpdatedMockZinCompiler {
  compile(zinSource) {
    const lines = zinSource.split("\n");
    let html = "";
    let i = 0;

    while (i < lines.length) {
      const line = lines[i];
      const trimmedLine = line.trim();

      if (!trimmedLine || trimmedLine.startsWith("//")) {
        i++;
        continue;
      }

      // Headers (h1, h2, h3, h4)
      if (trimmedLine.match(/^h[1-4](\s|$)/)) {
        const tag = trimmedLine.substring(0, 2);
        const level = tag[1];
        const content = this.parseTextWithStyles(
          trimmedLine.substring(2).trim()
        );
        html += `<h${level}>${content}</h${level}>\n`;
        i++;
      }
      // Paragraph
      else if (trimmedLine.match(/^p(\s|$)/)) {
        const content = this.parseTextWithStyles(
          trimmedLine.substring(1).trim()
        );
        // Handle multiline paragraphs
        let fullContent = content;
        i++;
        while (i < lines.length && lines[i].trim().match(/^:\s/)) {
          const continuationLine = lines[i].trim().substring(1).trim();
          fullContent += " " + this.escapeHtml(continuationLine);
          i++;
        }
        html += `<p>${fullContent}</p>\n`;
      }
      // Unordered list
      else if (trimmedLine.match(/^ul(\s|$)/)) {
        // Check if this starts a list block or is a single item
        const firstLineContent = this.parseTextWithStyles(
          trimmedLine.substring(2).trim()
        );
        html += "<ul>\n";
        html += `  <li>${firstLineContent}</li>\n`;
        i++;

        // Check for continuation list items
        while (i < lines.length && lines[i].match(/^\s+:\s/)) {
          const itemContent = this.parseTextWithStyles(
            lines[i].trim().substring(1).trim()
          );
          html += `  <li>${itemContent}</li>\n`;
          i++;
        }
        html += "</ul>\n";
      }
      // Ordered list
      else if (trimmedLine.match(/^ol(\s|$)/)) {
        // Check if this starts a list block or is a single item
        const firstLineContent = this.parseTextWithStyles(
          trimmedLine.substring(2).trim()
        );
        html += "<ol>\n";
        html += `  <li>${firstLineContent}</li>\n`;
        i++;

        // Check for continuation list items
        while (i < lines.length && lines[i].match(/^\s+:\s/)) {
          const itemContent = this.parseTextWithStyles(
            lines[i].trim().substring(1).trim()
          );
          html += `  <li>${itemContent}</li>\n`;
          i++;
        }
        html += "</ol>\n";
      }
      // Task list
      else if (trimmedLine.match(/^tl(\s|$)/)) {
        // Check if this starts a list block or is a single item
        const firstLineContent = this.parseTextWithStyles(
          trimmedLine.substring(2).trim()
        );
        html += '<ul class="todo-list">\n';
        html += `  <li>${firstLineContent}</li>\n`;
        i++;

        // Check for continuation list items
        while (i < lines.length && lines[i].match(/^\s+:\s/)) {
          const itemContent = this.parseTextWithStyles(
            lines[i].trim().substring(1).trim()
          );
          html += `  <li>${itemContent}</li>\n`;
          i++;
        }
        html += "</ul>\n";
      }
      // Code block
      else if (trimmedLine.match(/^cb(\s|$)/)) {
        const langMatch = trimmedLine.match(/lang\[([^\]]+)\]/);
        const lang = langMatch ? langMatch[1] : "text";

        html += `<pre><code class="language-${lang}">`;
        i++;

        // Read code block content
        while (i < lines.length && lines[i].startsWith(":")) {
          const codeLine = lines[i].substring(1).trim();
          html += this.escapeHtml(codeLine) + "\n";
          i++;
        }

        html += "</code></pre>\n";
      }
      // Table - FIXED ORDER HANDLING
      else if (trimmedLine.match(/^[tr](\s|$)/)) {
        const tableResult = this.parseTable(lines, i);
        html += tableResult.html;
        i = tableResult.nextIndex;
      }
      // Quote
      else if (trimmedLine.match(/^q(\s|$)/)) {
        const content = this.parseTextWithStyles(
          trimmedLine.substring(1).trim()
        );
        html += `<blockquote>${content}</blockquote>\n`;
        i++;
      } else {
        i++;
      }
    }

    return html;
  }

  parseTable(lines, startIndex) {
    let html = "<table>\n";
    let i = startIndex;

    // Continue parsing table rows until we hit a non-table line
    while (i < lines.length) {
      const line = lines[i].trim();

      if (line.match(/^t(\s|$)/)) {
        // Table header
        const cells = this.parseTableCells(line.substring(1).trim());
        html += "  <tr>\n";
        for (const cell of cells) {
          html += `    <th>${this.escapeHtml(cell)}</th>\n`;
        }
        html += "  </tr>\n";
        i++;
      } else if (line.match(/^r(\s|$)/)) {
        // Table row
        const cells = this.parseTableCells(line.substring(1).trim());
        html += "  <tr>\n";
        for (const cell of cells) {
          html += `    <td>${this.escapeHtml(cell)}</td>\n`;
        }
        html += "  </tr>\n";
        i++;
      } else if (!line) {
        // Skip empty lines within table
        i++;
      } else {
        // End of table
        break;
      }
    }

    html += "</table>\n";
    return { html, nextIndex: i };
  }

  parseTableCells(cellContent) {
    if (!cellContent.startsWith(":")) return [];

    const content = cellContent.substring(1).trim();
    const cells = content
      .split("|")
      .map((cell) => cell.trim())
      .filter((cell) => cell);
    return cells;
  }

  parseTextWithStyles(text) {
    // Handle case where there's no colon separator (plain text)
    const colonIndex = text.indexOf(" : ");
    if (colonIndex === -1) {
      // If no colon found, treat the entire text as content
      return this.escapeHtml(text.trim());
    }

    const stylesPart = text.substring(0, colonIndex);
    let content = text.substring(colonIndex + 3).trim(); // Skip ' : '

    // Apply styles like italic[0:8]
    const stylePattern = /(\w+)\[(\d+):(\d+)\]/g;
    let match;
    const styles = [];

    // Search for styles in the styles part
    while ((match = stylePattern.exec(stylesPart)) !== null) {
      styles.push({
        type: match[1],
        start: parseInt(match[2]),
        end: parseInt(match[3]),
      });
    }

    // Handle link styles separately
    const linkPattern = /link\[(\d+):(\d+)\].*?url\[([^\]]+)\]/;
    const linkMatch = stylesPart.match(linkPattern);
    if (linkMatch) {
      const start = parseInt(linkMatch[1]);
      const end = parseInt(linkMatch[2]);
      const url = linkMatch[3];

      // Escape HTML entities before applying styles
      content = this.escapeHtml(content);

      const before = content.substring(0, start);
      const linkText = content.substring(start, end);
      const after = content.substring(end);

      content = before + `<a href="${url}">${linkText}</a>` + after;
    } else {
      // Escape HTML entities before applying styles
      content = this.escapeHtml(content);
    }

    // Apply other styles in reverse order to handle nested styles
    styles.reverse().forEach((style) => {
      if (style.type === "link") return; // Skip link styles as they're handled above

      const before = content.substring(0, style.start);
      const styled = content.substring(style.start, style.end);
      const after = content.substring(style.end);

      let styledText;
      switch (style.type) {
        case "bold":
          styledText = `<strong>${styled}</strong>`;
          break;
        case "italic":
          styledText = `<em>${styled}</em>`;
          break;
        case "strike":
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
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#039;");
  }
};
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

  initZinCompiler: function () {
    console.log("Updated Mock Zin Compiler initialized");
    return 0;
  },

  getVersion: function () {
    return allocateString("zin-compiler-updated-mock 0.1.0");
  },

  compileZinToHTML: function (inputPtr, inputLen) {
    try {
      const input = textDecoder.decode(
        memoryBuffer.slice(inputPtr, inputPtr + inputLen)
      );
      const output = compiler.compile(input);
      return allocateString(output);
    } catch (error) {
      console.error("Compilation error:", error);
      return allocateString("ERROR: " + error.message);
    }
  },

  compileZinToJSON: function (inputPtr, inputLen) {
    try {
      const input = textDecoder.decode(
        memoryBuffer.slice(inputPtr, inputPtr + inputLen)
      );
      const html = compiler.compile(input);
      const result = JSON.stringify({ success: true, html: html });
      return allocateString(result);
    } catch (error) {
      console.error("JSON compilation error:", error);
      const result = JSON.stringify({ success: false, error: error.message });
      return allocateString(result);
    }
  },

  benchmarkCompilation: function (inputPtr, inputLen, iterations) {
    const input = textDecoder.decode(
      memoryBuffer.slice(inputPtr, inputPtr + inputLen)
    );
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
      avgTimePerIterationMs: totalTimeMs / iterations,
    });
    return allocateString(result);
  },

  malloc: function (size) {
    const ptr = memoryOffset;
    memoryOffset += size;
    return ptr;
  },

  free: function (ptr) {
    // No-op for this mock implementation
  },
};

// ES6 module export
export { exports };
export default { exports };

// CommonJS export for compatibility
if (typeof module !== "undefined" && module.exports) {
  module.exports = { exports };
}
