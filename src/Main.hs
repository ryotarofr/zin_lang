{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeBaseName, replaceExtension, takeDirectory, (</>))
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Control.Monad (when)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)

import Zin.Lexer
import Zin.Parser  
import Zin.Semantic
import Zin.CodeGen
import Zin.VirtualCodeGen
import Zin.Renderer
import Zin.Error
import Zin.AST
import Zin.IncrementalCompiler (CompilationMode(..), IncrementalCompiler(..), CompilationResult(..), CompilationPerformance(..), compileIncremental, initializeCompiler, loadCompilerState, saveCompilerState, analyzePerformance, getCompilerStats)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: zin-compiler [OPTIONS] <input.zin> [output.html]"
      putStrLn "       zin-compiler --help"
      exitFailure
    ["--help"] -> printHelp >> exitSuccess
    ["--version"] -> putStrLn versionInfo >> exitSuccess
    ["--incremental", inputFile] -> do
      let outputFile = replaceExtension inputFile ".html"
      compileFileIncremental IncrementalOnly inputFile outputFile
    ["--incremental", inputFile, outputFile] -> 
      compileFileIncremental IncrementalOnly inputFile outputFile
    ["--smart-incremental", inputFile] -> do
      let outputFile = replaceExtension inputFile ".html"
      compileFileIncremental SmartIncremental inputFile outputFile
    ["--smart-incremental", inputFile, outputFile] -> 
      compileFileIncremental SmartIncremental inputFile outputFile
    ["--fast-incremental", inputFile] -> do
      let outputFile = replaceExtension inputFile ".html"
      compileFileIncremental FastIncremental inputFile outputFile
    ["--fast-incremental", inputFile, outputFile] -> 
      compileFileIncremental FastIncremental inputFile outputFile
    ["--watch", inputFile] -> do
      let outputFile = replaceExtension inputFile ".html"
      watchAndCompile inputFile outputFile
    ["--stats", inputFile] -> showCompilationStats inputFile
    [inputFile] -> do
      let outputFile = replaceExtension inputFile ".html"
      compileFile inputFile outputFile
    [inputFile, outputFile] -> compileFile inputFile outputFile
    _ -> do
      putStrLn "Error: Invalid arguments"
      putStrLn "Usage: zin-compiler [OPTIONS] <input.zin> [output.html]"
      printUsage
      exitFailure

compileFile :: String -> String -> IO ()
compileFile inputPath outputPath = do
  fileExists <- doesFileExist inputPath
  when (not fileExists) $ do
    putStrLn $ "Error: File not found: " ++ inputPath
    exitFailure
    
  putStrLn $ "Compiling " ++ inputPath ++ " -> " ++ outputPath
  
  input <- TIO.readFile inputPath
  case compileZin input of
    Left err -> do
      let errorInfo = createError (T.pack inputPath) 1 1 err
      TIO.putStrLn (formatError errorInfo)
      exitFailure
    Right html -> do
      TIO.writeFile outputPath html
      putStrLn $ "Successfully compiled to " ++ outputPath
      exitSuccess

compileZin :: T.Text -> Either ParseError T.Text
compileZin input = do
  document <- parseZinFile input
  validatedDoc <- semanticAnalysis document
  let vdom = generateVDOMFromDocument validatedDoc
  return (renderToHTML vdom)

-- 増分コンパイル関数
compileFileIncremental :: CompilationMode -> String -> String -> IO ()
compileFileIncremental mode inputPath outputPath = do
  fileExists <- doesFileExist inputPath
  when (not fileExists) $ do
    putStrLn $ "Error: File not found: " ++ inputPath
    exitFailure
  
  putStrLn $ "Incremental compiling (" ++ show mode ++ ") " ++ inputPath ++ " -> " ++ outputPath
  
  -- 状態ファイルのパス
  let stateDir = takeDirectory inputPath </> ".zin_cache"
  let stateFile = stateDir </> takeBaseName inputPath ++ ".state"
  
  -- キャッシュディレクトリの作成
  createDirectoryIfMissing True stateDir
  
  -- 既存の状態を読み込み
  maybeCompiler <- loadCompilerState stateFile
  compiler <- case maybeCompiler of
    Just existingCompiler -> return $ existingCompiler { compilationMode = mode }
    Nothing -> initializeCompiler mode
  
  -- 入力ファイルを読み込み
  input <- TIO.readFile inputPath
  
  -- 増分コンパイル実行
  (result, newCompiler) <- compileIncremental input compiler
  
  case result of
    Left errors -> do
      mapM_ (\err -> do
        let errorInfo = createError (T.pack inputPath) 1 1 err
        TIO.putStrLn (formatError errorInfo)) errors
      exitFailure
    Right compResult -> do
      -- 結果をファイルに書き込み
      TIO.writeFile outputPath (resultHTML compResult)
      
      -- 統計情報を表示
      let perf = resultPerformance compResult
      putStrLn $ "Successfully compiled to " ++ outputPath
      putStrLn $ "Compilation time: " ++ show (perfTotalTime perf) ++ " seconds"
      putStrLn $ "Chunks processed: " ++ show (perfChunksProcessed perf)
      putStrLn $ "Chunks reused: " ++ show (perfChunksReused perf)
      putStrLn $ "Cache hit rate: " ++ show (perfCacheHitRate perf * 100) ++ "%"
      
      -- 状態を保存
      _ <- saveCompilerState stateFile newCompiler
      exitSuccess

-- ファイル監視とコンパイル
watchAndCompile :: String -> String -> IO ()
watchAndCompile inputPath outputPath = do
  putStrLn $ "Watching " ++ inputPath ++ " for changes..."
  putStrLn "Press Ctrl+C to stop watching"
  
  -- 初期コンパイル
  compiler <- initializeCompiler SmartIncremental
  initialContent <- TIO.readFile inputPath
  (_, newCompiler) <- compileIncremental initialContent compiler
  
  -- 簡易実装：実際の実装では file-watching ライブラリを使用
  putStrLn "File watching not fully implemented - performing one-time compilation"
  compileFileIncremental SmartIncremental inputPath outputPath

-- コンパイル統計の表示
showCompilationStats :: String -> IO ()
showCompilationStats inputPath = do
  let stateDir = takeDirectory inputPath </> ".zin_cache"
  let stateFile = stateDir </> takeBaseName inputPath ++ ".state"
  
  maybeCompiler <- loadCompilerState stateFile
  case maybeCompiler of
    Nothing -> putStrLn "No compilation history found for this file."
    Just compiler -> do
      analysis <- analyzePerformance compiler
      TIO.putStrLn analysis
      
      let (chunks, cacheEntries, hitRate, sessions) = getCompilerStats compiler
      putStrLn $ "Current chunks: " ++ show chunks
      putStrLn $ "Cache entries: " ++ show cacheEntries
      putStrLn $ "Overall hit rate: " ++ show (hitRate * 100) ++ "%"
      putStrLn $ "Total sessions: " ++ show sessions

printHelp :: IO ()
printHelp = do
  putStrLn "Zin Compiler - A compiler for the zin markup language with incremental compilation"
  putStrLn ""
  putStrLn "USAGE:"
  putStrLn "    zin-compiler [OPTIONS] <input.zin> [output.html]"
  putStrLn ""
  putStrLn "ARGS:"
  putStrLn "    <input.zin>     Input zin file to compile"
  putStrLn "    [output.html]   Output HTML file (optional, defaults to input name with .html extension)"
  putStrLn ""
  putStrLn "OPTIONS:"
  putStrLn "    --help                    Show this help message"
  putStrLn "    --version                 Show version information"
  putStrLn "    --incremental             Use incremental compilation"
  putStrLn "    --smart-incremental       Use smart incremental compilation (with dependency tracking)"
  putStrLn "    --fast-incremental        Use fast incremental compilation (minimal checks)"
  putStrLn "    --watch                   Watch file for changes and recompile automatically"
  putStrLn "    --stats                   Show compilation statistics for the file"
  putStrLn ""
  putStrLn "COMPILATION MODES:"
  putStrLn "    Default:           Full compilation every time (traditional)"
  putStrLn "    Incremental:       Only recompile changed parts"
  putStrLn "    Smart Incremental: Incremental with dependency tracking"
  putStrLn "    Fast Incremental:  Minimal change detection for speed"
  putStrLn ""
  putStrLn "EXAMPLES:"
  putStrLn "    zin-compiler document.zin"
  putStrLn "    zin-compiler --incremental document.zin"
  putStrLn "    zin-compiler --smart-incremental document.zin output.html"
  putStrLn "    zin-compiler --watch document.zin"
  putStrLn "    zin-compiler --stats document.zin"
  putStrLn ""
  putStrLn "The zin language supports:"
  putStrLn "  - Paragraphs (p)"
  putStrLn "  - Headers (h1, h2, h3, h4)"  
  putStrLn "  - Lists (ul, ol, tl)"
  putStrLn "  - Code blocks (cb)"
  putStrLn "  - Quotes (q)"
  putStrLn "  - Tables (t, r)"
  putStrLn "  - Text styling (bold, italic, strike, link)"
  putStrLn ""
  putStrLn "INCREMENTAL COMPILATION:"
  putStrLn "  Incremental compilation speeds up development by only recompiling"
  putStrLn "  changed parts of your document. The compiler maintains a cache"
  putStrLn "  in the .zin_cache/ directory next to your source file."
  putStrLn ""
  putStrLn "For more information, visit: https://zin-lang.org"

printUsage :: IO ()
printUsage = do
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  --help, --version"
  putStrLn "  --incremental, --smart-incremental, --fast-incremental"
  putStrLn "  --watch, --stats"

-- Version information
versionInfo :: String
versionInfo = "zin-compiler 0.2.0 (with incremental compilation)"

-- Additional CLI commands for future expansion
handleCommand :: String -> IO ()
handleCommand "version" = putStrLn versionInfo >> exitSuccess
handleCommand "check" = putStrLn "Syntax check mode not implemented yet" >> exitFailure
handleCommand cmd = do
  putStrLn $ "Unknown command: " ++ cmd
  putStrLn "Use --help for usage information"
  exitFailure