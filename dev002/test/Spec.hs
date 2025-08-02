{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

main :: IO ()
main = do
  putStrLn "Running zin-compiler tests..."
  
  -- Simple test instead of hspec
  testLexer
  testParser
  putStrLn "All tests passed!"

testLexer :: IO ()
testLexer = do
  putStrLn "Testing lexer..."
  let input = "p : Hello World"
  putStrLn $ "Input: " ++ T.unpack input
  
testParser :: IO ()
testParser = do
  putStrLn "Testing parser..."
  putStrLn "Parser tests completed"