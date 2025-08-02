{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeBaseName, replaceExtension)
import System.Directory (doesFileExist)
import Control.Monad (when)

import Zin.Lexer
import Zin.Parser  
import Zin.Semantic
import Zin.CodeGen
import Zin.VirtualCodeGen
import Zin.Renderer
import Zin.Error
import Zin.AST

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: zin-compiler <input.zin> [output.html]"
      putStrLn "       zin-compiler --help"
      exitFailure
    ["--help"] -> printHelp >> exitSuccess
    [inputFile] -> do
      let outputFile = replaceExtension inputFile ".html"
      compileFile inputFile outputFile
    [inputFile, outputFile] -> compileFile inputFile outputFile
    _ -> do
      putStrLn "Error: Too many arguments"
      putStrLn "Usage: zin-compiler <input.zin> [output.html]"
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

printHelp :: IO ()
printHelp = do
  putStrLn "Zin Compiler - A compiler for the zin markup language"
  putStrLn ""
  putStrLn "USAGE:"
  putStrLn "    zin-compiler <input.zin> [output.html]"
  putStrLn ""
  putStrLn "ARGS:"
  putStrLn "    <input.zin>     Input zin file to compile"
  putStrLn "    [output.html]   Output HTML file (optional, defaults to input name with .html extension)"
  putStrLn ""
  putStrLn "OPTIONS:"
  putStrLn "    --help          Show this help message"
  putStrLn ""
  putStrLn "EXAMPLES:"
  putStrLn "    zin-compiler document.zin"
  putStrLn "    zin-compiler document.zin output.html"
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
  putStrLn "For more information, visit: https://zin-lang.org"

-- Version information
versionInfo :: String
versionInfo = "zin-compiler 0.1.0"

-- Additional CLI commands for future expansion
handleCommand :: String -> IO ()
handleCommand "version" = putStrLn versionInfo >> exitSuccess
handleCommand "check" = putStrLn "Syntax check mode not implemented yet" >> exitFailure
handleCommand cmd = do
  putStrLn $ "Unknown command: " ++ cmd
  putStrLn "Use --help for usage information"
  exitFailure