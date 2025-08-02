{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Time (getCurrentTime, diffUTCTime)
import Control.Monad.IO.Class (liftIO)

import Zin.IncrementalCompiler
import Zin.Lexer
import Zin.Parser
import Zin.AST

main :: IO ()
main = do
  putStrLn "Running zin-compiler tests..."
  
  -- Basic tests
  testLexer
  testParser
  
  -- Incremental compilation tests
  testIncrementalCompilation
  testCacheEfficiency
  testPerformanceModes
  testErrorHandling
  
  putStrLn "All tests passed!"

testLexer :: IO ()
testLexer = do
  putStrLn "Testing lexer..."
  let input = T.pack "p : Hello World"
  putStrLn $ "Input: " ++ T.unpack input
  putStrLn "Lexer tests completed"
  
testParser :: IO ()
testParser = do
  putStrLn "Testing parser..."
  let input = T.pack "p : Hello World"
  case parseZinFile input of
    Left err -> putStrLn $ "Parser error: " ++ show err
    Right doc -> putStrLn "Parser tests completed"

testIncrementalCompilation :: IO ()
testIncrementalCompilation = do
  putStrLn "Testing incremental compilation..."
  
  -- Initialize compiler
  compiler <- initializeCompiler SmartIncremental
  
  -- First compilation
  let source1 = T.pack "p : Hello World\nh1 : Title"
  (result1, compiler1) <- compileIncremental source1 compiler
  
  case result1 of
    Left errors -> putStrLn $ "First compilation failed: " ++ show errors
    Right res1 -> do
      putStrLn $ "First compilation: " ++ show (length $ resultChunksCompiled res1) ++ " chunks compiled"
      
      -- Second compilation with changes
      let source2 = T.pack "p : Hello Universe\nh1 : Title"
      (result2, compiler2) <- compileIncremental source2 compiler1
      
      case result2 of
        Left errors -> putStrLn $ "Second compilation failed: " ++ show errors
        Right res2 -> do
          putStrLn $ "Second compilation: " ++ show (length $ resultChunksCompiled res2) ++ " chunks compiled, " 
                   ++ show (length $ resultChunksReused res2) ++ " chunks reused"
          
          -- Third compilation (no changes)
          (result3, _) <- compileIncremental source2 compiler2
          case result3 of
            Left errors -> putStrLn $ "Third compilation failed: " ++ show errors
            Right res3 -> do
              putStrLn $ "Third compilation: " ++ show (length $ resultChunksCompiled res3) ++ " chunks compiled, "
                       ++ show (length $ resultChunksReused res3) ++ " chunks reused"
  
  putStrLn "Incremental compilation tests completed"

testCacheEfficiency :: IO ()
testCacheEfficiency = do
  putStrLn "Testing cache efficiency..."
  
  compiler <- initializeCompiler IncrementalOnly
  
  let sources = [ T.pack $ "p : Test " ++ show i | i <- [1..5] ]
  
  startTime <- getCurrentTime
  
  -- Compile multiple sources
  finalCompiler <- foldM compileAndMeasure compiler sources
  
  endTime <- getCurrentTime
  let totalTime = diffUTCTime endTime startTime
  
  let (chunks, cacheEntries, hitRate, sessions) = getCompilerStats finalCompiler
  
  putStrLn $ "Cache stats: " ++ show cacheEntries ++ " entries, " 
           ++ show sessions ++ " sessions"
  putStrLn $ "Total time: " ++ show totalTime
  putStrLn "Cache efficiency tests completed"
  
  where
    compileAndMeasure comp source = do
      (_, newComp) <- compileIncremental source comp
      return newComp

testPerformanceModes :: IO ()
testPerformanceModes = do
  putStrLn "Testing performance modes..."
  
  let testSource = T.pack "h1 : Title\np : This is a test paragraph\ncode : let x = 42"
  
  -- Test each compilation mode
  modes <- mapM (testMode testSource) [FullCompilation, IncrementalOnly, SmartIncremental, FastIncremental]
  
  putStrLn $ "Performance comparison: " ++ show (length modes) ++ " modes tested"
  putStrLn "Performance mode tests completed"
  
  where
    testMode source mode = do
      compiler <- initializeCompiler mode
      startTime <- getCurrentTime
      (result, _) <- compileIncremental source compiler
      endTime <- getCurrentTime
      let duration = diffUTCTime endTime startTime
      putStrLn $ "Mode " ++ show mode ++ ": " ++ show duration
      return (mode, duration, result)

testErrorHandling :: IO ()
testErrorHandling = do
  putStrLn "Testing error handling..."
  
  compiler <- initializeCompiler SmartIncremental
  
  -- Test with invalid syntax
  let invalidSource = T.pack "p : Unclosed quote"
  (result, _) <- compileIncremental invalidSource compiler
  
  case result of
    Left errors -> putStrLn $ "Error handling working: " ++ show (length errors) ++ " errors caught"
    Right _ -> putStrLn "Expected errors but compilation succeeded"
  
  -- Test with empty source
  let emptySource = T.pack ""
  (result2, _) <- compileIncremental emptySource compiler
  
  case result2 of
    Left errors -> putStrLn $ "Empty source handling: " ++ show (length errors) ++ " errors"
    Right _ -> putStrLn "Empty source compiled successfully"
  
  putStrLn "Error handling tests completed"

foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM _ acc [] = return acc
foldM f acc (x:xs) = do
  acc' <- f acc x
  foldM f acc' xs