/**
 * Zin Compiler WebAssembly JavaScript Interface
 * 
 * This module provides a JavaScript interface for the Zin compiler compiled to WebAssembly.
 * It allows you to compile Zin markup language to HTML directly in the browser.
 */

class ZinCompiler {
  constructor() {
    this.wasmModule = null;
    this.initialized = false;
  }

  /**
   * Initialize the WASM module
   * @param {string} wasmPath - Path to the WASM file
   * @returns {Promise<void>}
   */
  async init(wasmPath = './zin-compiler-wasm.wasm') {
    try {
      // First try to load as JavaScript module (for mock/development)
      if (wasmPath.endsWith('.js')) {
        try {
          const mockModule = await import(wasmPath);
          this.wasmModule = { exports: mockModule.exports || mockModule.default?.exports || mockModule };
          
          // Initialize the compiler
          if (this.wasmModule.exports.initZinCompiler) {
            this.wasmModule.exports.initZinCompiler();
          }
          
          this.initialized = true;
          console.log('Zin Compiler mock module initialized successfully');
          return;
        } catch (mockError) {
          console.warn('Failed to load mock module, trying WASM:', mockError);
          throw mockError; // Re-throw since this was explicitly a JS file
        }
      }
      
      // Load the WASM module
      const wasmResponse = await fetch(wasmPath);
      const wasmBytes = await wasmResponse.arrayBuffer();
      
      // Instantiate the WASM module
      const wasmModule = await WebAssembly.instantiate(wasmBytes, {
        // Import objects if needed
        env: {
          // Memory and other imports can be defined here
        }
      });
      
      this.wasmModule = wasmModule.instance;
      
      // Initialize the compiler
      if (this.wasmModule.exports.initZinCompiler) {
        this.wasmModule.exports.initZinCompiler();
      }
      
      this.initialized = true;
      console.log('Zin Compiler WASM module initialized successfully');
    } catch (error) {
      console.error('Failed to initialize Zin Compiler WASM module:', error);
      throw error;
    }
  }

  /**
   * Check if the compiler is initialized
   * @returns {boolean}
   */
  isInitialized() {
    return this.initialized && this.wasmModule !== null;
  }

  /**
   * Get the compiler version
   * @returns {string}
   */
  getVersion() {
    if (!this.isInitialized()) {
      throw new Error('Compiler not initialized. Call init() first.');
    }
    
    const versionPtr = this.wasmModule.exports.getVersion();
    return this.readCString(versionPtr);
  }

  /**
   * Compile Zin source code to HTML
   * @param {string} zinSource - The Zin source code to compile
   * @returns {string} - The compiled HTML output
   */
  compile(zinSource) {
    if (!this.isInitialized()) {
      throw new Error('Compiler not initialized. Call init() first.');
    }

    try {
      // Convert string to bytes
      const inputBytes = new TextEncoder().encode(zinSource);
      
      // Allocate memory for input
      const inputPtr = this.wasmModule.exports.malloc(inputBytes.length);
      const inputView = new Uint8Array(this.wasmModule.exports.memory.buffer, inputPtr, inputBytes.length);
      inputView.set(inputBytes);
      
      // Call the compile function
      const outputPtr = this.wasmModule.exports.compileZinToHTML(inputPtr, inputBytes.length);
      
      // Read the output
      const output = this.readCString(outputPtr);
      
      // Free allocated memory
      this.wasmModule.exports.free(inputPtr);
      
      return output;
    } catch (error) {
      console.error('Compilation error:', error);
      throw new Error(`Compilation failed: ${error.message}`);
    }
  }

  /**
   * Compile Zin source code and return JSON response with success/error info
   * @param {string} zinSource - The Zin source code to compile
   * @returns {Object} - JSON object with compilation result
   */
  compileToJSON(zinSource) {
    if (!this.isInitialized()) {
      throw new Error('Compiler not initialized. Call init() first.');
    }

    try {
      // Convert string to bytes
      const inputBytes = new TextEncoder().encode(zinSource);
      
      // Allocate memory for input
      const inputPtr = this.wasmModule.exports.malloc(inputBytes.length);
      const inputView = new Uint8Array(this.wasmModule.exports.memory.buffer, inputPtr, inputBytes.length);
      inputView.set(inputBytes);
      
      // Call the compile function
      const outputPtr = this.wasmModule.exports.compileZinToJSON(inputPtr, inputBytes.length);
      
      // Read the output
      const jsonString = this.readCString(outputPtr);
      
      // Free allocated memory
      this.wasmModule.exports.free(inputPtr);
      
      return JSON.parse(jsonString);
    } catch (error) {
      console.error('JSON compilation error:', error);
      return {
        success: false,
        error: `Compilation failed: ${error.message}`
      };
    }
  }

  /**
   * Benchmark the compilation performance
   * @param {string} zinSource - The Zin source code to benchmark
   * @param {number} iterations - Number of iterations to run
   * @returns {Object} - Benchmark results
   */
  benchmark(zinSource, iterations = 100) {
    if (!this.isInitialized()) {
      throw new Error('Compiler not initialized. Call init() first.');
    }

    try {
      const startTime = performance.now();
      
      // Convert string to bytes
      const inputBytes = new TextEncoder().encode(zinSource);
      
      // Allocate memory for input
      const inputPtr = this.wasmModule.exports.malloc(inputBytes.length);
      const inputView = new Uint8Array(this.wasmModule.exports.memory.buffer, inputPtr, inputBytes.length);
      inputView.set(inputBytes);
      
      // Call the benchmark function
      const outputPtr = this.wasmModule.exports.benchmarkCompilation(inputPtr, inputBytes.length, iterations);
      
      // Read the output
      const jsonString = this.readCString(outputPtr);
      const result = JSON.parse(jsonString);
      
      const endTime = performance.now();
      
      // Free allocated memory
      this.wasmModule.exports.free(inputPtr);
      
      // Add JavaScript timing information
      result.totalTimeMs = endTime - startTime;
      result.avgTimePerIterationMs = result.totalTimeMs / iterations;
      
      return result;
    } catch (error) {
      console.error('Benchmark error:', error);
      throw new Error(`Benchmark failed: ${error.message}`);
    }
  }

  /**
   * Read a C-style null-terminated string from WASM memory
   * @param {number} ptr - Pointer to the string in WASM memory
   * @returns {string}
   */
  readCString(ptr) {
    const memory = new Uint8Array(this.wasmModule.exports.memory.buffer);
    let length = 0;
    
    // Find the null terminator
    while (memory[ptr + length] !== 0) {
      length++;
    }
    
    // Extract the string bytes
    const stringBytes = memory.slice(ptr, ptr + length);
    
    // Convert to string
    return new TextDecoder().decode(stringBytes);
  }

  /**
   * Clean up resources
   */
  dispose() {
    this.wasmModule = null;
    this.initialized = false;
  }
}

// Export for different module systems
if (typeof module !== 'undefined' && module.exports) {
  // Node.js
  module.exports = ZinCompiler;
} else if (typeof window !== 'undefined') {
  // Browser global
  window.ZinCompiler = ZinCompiler;
}

// ES6 module export
export default ZinCompiler;