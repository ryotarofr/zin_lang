{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Zin.Chunking
  ( ChunkingStrategy (..),
    ChunkBoundary (..),
    chunkifyText,
    chunkifyLines,
    mergeSmallChunks,
    splitLargeChunks,
    optimizeChunks,
    detectChunkBoundaries,
    calculateChunkDependencies,
  )
where

import Control.Monad.State
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Zin.Incremental
import Zin.Lexer (IndentedLine (..), lexer)

-- チャンク分割戦略
data ChunkingStrategy
  = ByTopLevelBlocks -- トップレベルブロック単位
  | ByLogicalSections -- 論理的なセクション単位
  | ByLineCount Int -- 指定行数単位
  | ByCharacterCount Int -- 指定文字数単位
  | Adaptive -- 内容に応じた適応的分割
  deriving (Show, Eq)

-- チャンク境界の種類
data ChunkBoundary
  = BlockBoundary -- ブロック境界
  | SectionBoundary -- セクション境界
  | IndentBoundary -- インデント境界
  | ForcedBoundary -- 強制境界
  deriving (Show, Eq)

-- テキストをチャンクに分割
chunkifyText :: ChunkingStrategy -> Text -> [TextChunk]
chunkifyText strategy text =
  let indentedLines = lexer text
   in chunkifyLines strategy indentedLines

-- 行をチャンクに分割
chunkifyLines :: ChunkingStrategy -> [IndentedLine] -> [TextChunk]
chunkifyLines strategy lines =
  case strategy of
    ByTopLevelBlocks -> chunkByTopLevelBlocks lines
    ByLogicalSections -> chunkByLogicalSections lines
    ByLineCount n -> chunkByLineCount n lines
    ByCharacterCount n -> chunkByCharacterCount n lines
    Adaptive -> chunkAdaptive lines

-- トップレベルブロック単位でチャンク分割
chunkByTopLevelBlocks :: [IndentedLine] -> [TextChunk]
chunkByTopLevelBlocks lines =
  let boundaries = detectTopLevelBoundaries lines
      chunks = splitByBoundaries boundaries lines
   in map createChunkFromLines chunks

-- 論理的セクション単位でチャンク分割
chunkByLogicalSections :: [IndentedLine] -> [TextChunk]
chunkByLogicalSections lines =
  let boundaries = detectLogicalBoundaries lines
      chunks = splitByBoundaries boundaries lines
   in map createChunkFromLines chunks

-- 行数単位でチャンク分割
chunkByLineCount :: Int -> [IndentedLine] -> [TextChunk]
chunkByLineCount chunkSize lines =
  let chunkedLines = chunksOf chunkSize lines
   in map createChunkFromLines chunkedLines

-- 文字数単位でチャンク分割
chunkByCharacterCount :: Int -> [IndentedLine] -> [TextChunk]
chunkByCharacterCount maxChars lines =
  let chunks = splitByCharacterCount maxChars lines
   in map createChunkFromLines chunks

-- 適応的チャンク分割
chunkAdaptive :: [IndentedLine] -> [TextChunk]
chunkAdaptive lines =
  let -- まずトップレベルブロック単位で分割
      topLevelChunks = chunkByTopLevelBlocks lines
      -- 小さすぎるチャンクをマージ
      mergedChunks = mergeSmallChunks 50 topLevelChunks
      -- 大きすぎるチャンクを分割
      optimizedChunks = concatMap (splitIfTooLarge 500) mergedChunks
   in optimizedChunks

-- トップレベルブロック境界を検出
detectTopLevelBoundaries :: [IndentedLine] -> [Int]
detectTopLevelBoundaries lines =
  let lineNumbers = map lineNumber lines
      topLevelLines = filter (\line -> indentLevel line == 0 && not (T.null (content line))) lines
      topLevelNumbers = map lineNumber topLevelLines
   in topLevelNumbers

-- 論理的境界を検出
detectLogicalBoundaries :: [IndentedLine] -> [Int]
detectLogicalBoundaries lines =
  let linesWithBoundaries = [(lineNumber line, detectBoundaryTypes line) | line <- lines]
      filteredLines = filter (not . null . snd) linesWithBoundaries
   in map fst filteredLines
  where
    detectBoundaryTypes line =
      let lineContent = T.strip (content line)
       in [BlockBoundary | isHeaderLine lineContent || isListStart lineContent || isTableStart lineContent]

-- ヘッダー行かどうか判定
isHeaderLine :: Text -> Bool
isHeaderLine text = any (`T.isPrefixOf` text) ["h1:", "h2:", "h3:", "h4:"]

-- リスト開始行かどうか判定
isListStart :: Text -> Bool
isListStart text = any (`T.isPrefixOf` text) ["ul:", "ol:", "tl:"]

-- テーブル開始行かどうか判定
isTableStart :: Text -> Bool
isTableStart = T.isPrefixOf "t:"

-- 境界に基づいて分割
splitByBoundaries :: [Int] -> [IndentedLine] -> [[IndentedLine]]
splitByBoundaries [] lines = [lines]
splitByBoundaries boundaries lines =
  let sortedBoundaries = sortBy compare boundaries
   in splitByBoundariesHelper sortedBoundaries lines

splitByBoundariesHelper :: [Int] -> [IndentedLine] -> [[IndentedLine]]
splitByBoundariesHelper [] lines = [lines]
splitByBoundariesHelper [_] lines = [lines]
splitByBoundariesHelper (b1 : b2 : rest) lines =
  let (chunk, remaining) = span (\line -> lineNumber line < b2) lines
   in chunk : splitByBoundariesHelper (b2 : rest) remaining

-- 文字数で分割
splitByCharacterCount :: Int -> [IndentedLine] -> [[IndentedLine]]
splitByCharacterCount maxChars lines =
  evalState (splitByCharacterCountM maxChars lines) 0

splitByCharacterCountM :: Int -> [IndentedLine] -> State Int [[IndentedLine]]
splitByCharacterCountM _ [] = return []
splitByCharacterCountM maxChars lines = do
  currentCount <- get
  let (chunk, remaining) = takeWhileCharCount maxChars currentCount lines
  put 0 -- リセット
  rest <- splitByCharacterCountM maxChars remaining
  return (chunk : rest)

takeWhileCharCount :: Int -> Int -> [IndentedLine] -> ([IndentedLine], [IndentedLine])
takeWhileCharCount maxChars currentCount lines =
  case lines of
    [] -> ([], [])
    (line : rest) ->
      let lineLength = T.length (content line)
          newCount = currentCount + lineLength
       in if newCount <= maxChars
            then
              let (taken, remaining) = takeWhileCharCount maxChars newCount rest
               in (line : taken, remaining)
            else ([], lines)

-- 行をTextChunkに変換
createChunkFromLines :: [IndentedLine] -> TextChunk
createChunkFromLines [] =
  let emptyId = createChunkId ""
   in TextChunk emptyId "" 0 0 0
createChunkFromLines lines =
  let startLine = minimum $ map lineNumber lines
      endLine = maximum $ map lineNumber lines
      minIndent = minimum $ map indentLevel lines
      combinedContent = T.unlines $ map content lines
      chunkId = createChunkId combinedContent
   in TextChunk chunkId combinedContent startLine endLine minIndent

-- ユーティリティ: リストを指定サイズのチャンクに分割
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n list =
  let (chunk, rest) = splitAt n list
   in chunk : chunksOf n rest

-- 小さなチャンクをマージ
mergeSmallChunks :: Int -> [TextChunk] -> [TextChunk]
mergeSmallChunks minSize chunks =
  mergeContinuousSmallChunks minSize chunks

mergeContinuousSmallChunks :: Int -> [TextChunk] -> [TextChunk]
mergeContinuousSmallChunks _ [] = []
mergeContinuousSmallChunks _ [chunk] = [chunk]
mergeContinuousSmallChunks minSize (chunk1 : chunk2 : rest) =
  let size1 = T.length (chunkContent chunk1)
      size2 = T.length (chunkContent chunk2)
   in if size1 < minSize && size2 < minSize && canMergeChunks chunk1 chunk2
        then
          let mergedChunk = mergeChunks chunk1 chunk2
           in mergeContinuousSmallChunks minSize (mergedChunk : rest)
        else chunk1 : mergeContinuousSmallChunks minSize (chunk2 : rest)

-- チャンクがマージ可能かチェック
canMergeChunks :: TextChunk -> TextChunk -> Bool
canMergeChunks chunk1 chunk2 =
  -- 連続する行でかつ、インデントレベルが同じまたは近い
  chunkEndLine chunk1 + 1 == chunkStartLine chunk2
    && abs (chunkIndentLevel chunk1 - chunkIndentLevel chunk2) <= 1

-- チャンクをマージ
mergeChunks :: TextChunk -> TextChunk -> TextChunk
mergeChunks chunk1 chunk2 =
  let combinedContent = chunkContent chunk1 <> "\n" <> chunkContent chunk2
      newChunkId = createChunkId combinedContent
      newStartLine = min (chunkStartLine chunk1) (chunkStartLine chunk2)
      newEndLine = max (chunkEndLine chunk1) (chunkEndLine chunk2)
      newIndentLevel = min (chunkIndentLevel chunk1) (chunkIndentLevel chunk2)
   in TextChunk newChunkId combinedContent newStartLine newEndLine newIndentLevel

-- 大きすぎるチャンクを分割
splitLargeChunks :: Int -> [TextChunk] -> [TextChunk]
splitLargeChunks maxSize chunks =
  concatMap (splitIfTooLarge maxSize) chunks

splitIfTooLarge :: Int -> TextChunk -> [TextChunk]
splitIfTooLarge maxSize chunk =
  let content = chunkContent chunk
      contentLength = T.length content
   in if contentLength <= maxSize
        then [chunk]
        else splitChunkBySize maxSize chunk

splitChunkBySize :: Int -> TextChunk -> [TextChunk]
splitChunkBySize maxSize chunk =
  let lines = T.lines (chunkContent chunk)
      lineCount = length lines
      avgLineLength = if lineCount > 0 then T.length (chunkContent chunk) `div` lineCount else 0
      estimatedLinesPerChunk = if avgLineLength > 0 then maxSize `div` avgLineLength else 10
      chunkedLines = chunksOf (max 1 estimatedLinesPerChunk) lines
   in map (createChunkFromTextLines (chunkStartLine chunk)) chunkedLines

createChunkFromTextLines :: Int -> [Text] -> TextChunk
createChunkFromTextLines startLineOffset textLines =
  let combinedContent = T.unlines textLines
      chunkId = createChunkId combinedContent
      lineCount = length textLines
      endLine = startLineOffset + lineCount - 1
      -- インデントレベルは最初の行から推定
      indentLevel = if null textLines then 0 else T.length $ T.takeWhile (\c -> c == ' ' || c == '\t') (head textLines)
   in TextChunk chunkId combinedContent startLineOffset endLine indentLevel

-- チャンクを最適化
optimizeChunks :: [TextChunk] -> [TextChunk]
optimizeChunks chunks =
  let -- 小さなチャンクをマージ
      merged = mergeSmallChunks 50 chunks
      -- 大きなチャンクを分割
      split = splitLargeChunks 1000 merged
      -- 空のチャンクを除去
      filtered = filter (not . T.null . chunkContent) split
   in filtered

-- チャンク境界を検出
detectChunkBoundaries :: [IndentedLine] -> [(Int, ChunkBoundary)]
detectChunkBoundaries lines =
  concatMap detectLineBoundary lines
  where
    detectLineBoundary line =
      let lineNum = lineNumber line
          lineContent = T.strip (content line)
          indent = indentLevel line
       in if
            | indent == 0 && not (T.null lineContent) -> [(lineNum, BlockBoundary)]
            | isHeaderLine lineContent -> [(lineNum, SectionBoundary)]
            | isListStart lineContent -> [(lineNum, BlockBoundary)]
            | isTableStart lineContent -> [(lineNum, BlockBoundary)]
            | otherwise -> []

-- チャンクの依存関係を計算
calculateChunkDependencies :: [TextChunk] -> [(ChunkId, [ChunkId])]
calculateChunkDependencies chunks =
  map (\chunk -> (chunkId chunk, findDependencies chunk chunks)) chunks

-- 特定のチャンクの依存関係を検出
findDependencies :: TextChunk -> [TextChunk] -> [ChunkId]
findDependencies targetChunk allChunks =
  let targetContent = chunkContent targetChunk
      referencedIds = extractReferences targetContent
   in filter (/= chunkId targetChunk) $
        map chunkId $
          filter (\chunk -> chunkId chunk `elem` referencedIds) allChunks

-- コンテンツから参照を抽出（簡易実装）
extractReferences :: Text -> [ChunkId]
extractReferences content =
  -- 実際の実装では、リンクや参照構文を解析
  -- ここでは簡単な例として空リストを返す
  []