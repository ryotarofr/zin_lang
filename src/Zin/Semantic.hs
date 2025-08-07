{-# LANGUAGE OverloadedStrings #-}

module Zin.Semantic
  ( validateDocument,
    validateBlock,
    validateStyles,
    semanticAnalysis,
  )
where

import Control.Monad.Except
import Data.List (group, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Zin.AST
import Zin.Error

type SemanticAnalyzer = ExceptT ParseError IO

semanticAnalysis :: Document -> Either ParseError Document
semanticAnalysis doc =
  case validateDocument doc of
    Left err -> Left err
    Right validDoc -> Right validDoc

validateDocument :: Document -> Either ParseError Document
validateDocument (Document blocks) = do
  validatedBlocks <- mapM validateBlock blocks
  return (Document validatedBlocks)

validateBlock :: Block -> Either ParseError Block
validateBlock block = case block of
  Paragraph styles text -> do
    validatedStyles <- validateStyles styles text
    return (Paragraph validatedStyles text)
  Header level styles text -> do
    when (level < 1 || level > 4) $
      Left (UnknownTag ("h" <> T.pack (show level)))
    validatedStyles <- validateStyles styles text
    return (Header level validatedStyles text)
  List listType items -> do
    validatedItems <- mapM validateListItem items
    return (List listType validatedItems)
  CodeBlock lang code -> do
    when (T.null code) $
      Left InvalidCodeBlockFormat
    return (CodeBlock lang code)
  Quote styles text -> do
    validatedStyles <- validateStyles styles text
    return (Quote validatedStyles text)
  Table tables -> do
    validateTableStructure tables
    return (Table tables)

validateListItem :: ListItem -> Either ParseError ListItem
validateListItem (ListItem styles text) = do
  validatedStyles <- validateStyles styles text
  return (ListItem validatedStyles text)

validateStyles :: [Style] -> Text -> Either ParseError [Style]
validateStyles styles text = do
  mapM (validateStyle text) styles
  checkStyleOverlaps styles
  return styles

validateStyle :: Text -> Style -> Either ParseError Style
validateStyle text style = do
  let (start, end) = getStyleRange style
  let textLength = T.length text

  when (start < 0) $
    Left StyleRangeOutOfBounds

  when (end > textLength) $
    Left StyleRangeOutOfBounds

  when (start > end) $
    Left StyleRangeOutOfBounds

  case style of
    Link _ url -> when (T.null url) $ Left URLNotFound
    _ -> return ()

  return style

checkStyleOverlaps :: [Style] -> Either ParseError ()
checkStyleOverlaps styles = do
  let ranges = map getStyleRange styles
  let sortedRanges = sort ranges
  checkOverlapsInSorted sortedRanges
  return ()

checkOverlapsInSorted :: [(Int, Int)] -> Either ParseError ()
checkOverlapsInSorted [] = return ()
checkOverlapsInSorted [_] = return ()
checkOverlapsInSorted ((s1, e1) : (s2, e2) : rest) = do
  when (e1 > s2) $
    Left (InvalidStyleRange "Overlapping style ranges not allowed")
  checkOverlapsInSorted ((s2, e2) : rest)

validateTableStructure :: [Table] -> Either ParseError ()
validateTableStructure [] = return () -- 空のテーブルも許可
validateTableStructure tables = do
  let (headers, rows) = partitionTables tables
  -- ヘッダーが存在する場合のみ列数チェック
  case headers of
    [] -> return () -- ヘッダーなしも暫定的に許可
    [HeaderRow headerCols] -> do
      let expectedCols = length headerCols
      mapM_ (validateRowColumns expectedCols) rows
    _ -> return () -- 複数ヘッダーも暫定的に許可

partitionTables :: [Table] -> ([Table], [Table])
partitionTables = foldr classify ([], [])
  where
    classify h@(HeaderRow _) (headers, rows) = (h : headers, rows)
    classify r@(DataRow _) (headers, rows) = (headers, r : rows)

validateRowColumns :: Int -> Table -> Either ParseError ()
validateRowColumns expected (DataRow cols) = do
  when (length cols /= expected) $ Left InconsistentTableColumns
validateRowColumns _ (HeaderRow _) = return ()

-- Helper function for error context
when :: Bool -> Either ParseError () -> Either ParseError ()
when True action = action
when False _ = return ()

-- Text processing utilities for semantic analysis
normalizeText :: Text -> Text
normalizeText = T.strip . T.unwords . T.words

validateTextNotEmpty :: Text -> Either ParseError ()
validateTextNotEmpty text = do
  when (T.null (normalizeText text)) $ Left EmptyInput

-- Style application with validation
applyValidatedStyles :: [Style] -> Text -> Either ParseError Text
applyValidatedStyles styles text = do
  validatedStyles <- validateStyles styles text
  return $ foldr applyStyleToText text validatedStyles

-- Check for style conflicts
hasStyleConflicts :: [Style] -> Bool
hasStyleConflicts styles =
  let ranges = map getStyleRange styles
      sortedRanges = sort ranges
   in hasOverlaps sortedRanges

hasOverlaps :: [(Int, Int)] -> Bool
hasOverlaps [] = False
hasOverlaps [_] = False
hasOverlaps ((_, e1) : (s2, _) : rest) = e1 > s2 || hasOverlaps rest