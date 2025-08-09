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
import Data.List (sort)
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
chunkifyLines strategy indentedLines =
  case strategy of
    ByTopLevelBlocks -> chunkByTopLevelBlocks indentedLines
    ByLogicalSections -> chunkByLogicalSections indentedLines
    ByLineCount n -> chunkByLineCount n indentedLines
    ByCharacterCount n -> chunkByCharacterCount n indentedLines
    Adaptive -> chunkAdaptive indentedLines

-- トップレベルブロック単位でチャンク分割
chunkByTopLevelBlocks :: [IndentedLine] -> [TextChunk]
chunkByTopLevelBlocks indentedLines =
  let boundaries = detectTopLevelBoundaries indentedLines
      chunks = splitByBoundaries boundaries indentedLines
   in map createChunkFromLines chunks

-- 論理的セクション単位でチャンク分割
chunkByLogicalSections :: [IndentedLine] -> [TextChunk]
chunkByLogicalSections indentedLines =
  let boundaries = detectLogicalBoundaries indentedLines
      chunks = splitByBoundaries boundaries indentedLines
   in map createChunkFromLines chunks

-- 行数単位でチャンク分割
chunkByLineCount :: Int -> [IndentedLine] -> [TextChunk]
chunkByLineCount chunkSize indentedLines =
  let chunkedLines = chunksOf chunkSize indentedLines
   in map createChunkFromLines chunkedLines

-- 文字数単位でチャンク分割
chunkByCharacterCount :: Int -> [IndentedLine] -> [TextChunk]
chunkByCharacterCount maxChars indentedLines =
  let chunks = splitByCharacterCount maxChars indentedLines
   in map createChunkFromLines chunks

-- 適応的チャンク分割
chunkAdaptive :: [IndentedLine] -> [TextChunk]
chunkAdaptive indentedLines =
  let -- まずトップレベルブロック単位で分割
      topLevelChunks = chunkByTopLevelBlocks indentedLines
      -- 小さすぎるチャンクをマージ
      mergedChunks = mergeSmallChunks 50 topLevelChunks
      -- 大きすぎるチャンクを分割
      optimizedChunks = concatMap (splitIfTooLarge 500) mergedChunks
   in optimizedChunks

-- トップレベルブロック境界を検出
detectTopLevelBoundaries :: [IndentedLine] -> [Int]
detectTopLevelBoundaries indentedLines =
  let topLevelLines = filter (\line -> indentLevel line == 0 && not (T.null (content line))) indentedLines
      topLevelNumbers = map lineNumber topLevelLines
   in topLevelNumbers

-- 論理的境界を検出
detectLogicalBoundaries :: [IndentedLine] -> [Int]
detectLogicalBoundaries indentedLines =
  let linesWithBoundaries = [(lineNumber line, detectBoundaryTypes line) | line <- indentedLines]
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
splitByBoundaries [] indentedLines = [indentedLines]
splitByBoundaries boundaries indentedLines =
  let sortedBoundaries = sort boundaries
   in splitByBoundariesHelper sortedBoundaries indentedLines

splitByBoundariesHelper :: [Int] -> [IndentedLine] -> [[IndentedLine]]
splitByBoundariesHelper [] indentedLines = [indentedLines]
splitByBoundariesHelper [_] indentedLines = [indentedLines]
splitByBoundariesHelper (_ : b2 : rest) indentedLines =
  let (chunk, remaining) = span (\line -> lineNumber line < b2) indentedLines
   in chunk : splitByBoundariesHelper (b2 : rest) remaining

-- 文字数で分割
splitByCharacterCount :: Int -> [IndentedLine] -> [[IndentedLine]]
splitByCharacterCount maxChars indentedLines =
  evalState (splitByCharacterCountM maxChars indentedLines) 0

splitByCharacterCountM :: Int -> [IndentedLine] -> State Int [[IndentedLine]]
splitByCharacterCountM _ [] = return []
splitByCharacterCountM maxChars indentedLines = do
  currentCount <- get
  let (chunk, remaining) = takeWhileCharCount maxChars currentCount indentedLines
  put 0 -- リセット
  rest <- splitByCharacterCountM maxChars remaining
  return (chunk : rest)

takeWhileCharCount :: Int -> Int -> [IndentedLine] -> ([IndentedLine], [IndentedLine])
takeWhileCharCount maxChars currentCount indentedLines =
  case indentedLines of
    [] -> ([], [])
    (line : rest) ->
      let lineLength = T.length (content line)
          newCount = currentCount + lineLength
       in if newCount <= maxChars
            then
              let (taken, remaining) = takeWhileCharCount maxChars newCount rest
               in (line : taken, remaining)
            else ([], indentedLines)

-- 行をTextChunkに変換
createChunkFromLines :: [IndentedLine] -> TextChunk
createChunkFromLines [] =
  let emptyId = createChunkId ""
   in TextChunk emptyId "" 0 0 0
createChunkFromLines indentedLines =
  let startLine = minimum $ map lineNumber indentedLines
      endLine = maximum $ map lineNumber indentedLines
      minIndent = minimum $ map indentLevel indentedLines
      combinedContent = T.unlines $ map content indentedLines
      newChunkId = createChunkId combinedContent
   in TextChunk newChunkId combinedContent startLine endLine minIndent

-- ユーティリティ: リストを指定サイズのチャンクに分割
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n list =
  let (chunk, rest) = splitAt n list
   in chunk : chunksOf n rest

-- 小さなチャンクをマージ
mergeSmallChunks :: Int -> [TextChunk] -> [TextChunk]
mergeSmallChunks _ [] = []
mergeSmallChunks _ [chunk] = [chunk]
mergeSmallChunks minSize (chunk1 : chunk2 : rest) =
  let size1 = T.length (chunkContent chunk1)
      size2 = T.length (chunkContent chunk2)
   in if size1 < minSize && size2 < minSize && canMergeChunks chunk1 chunk2
        then
          let mergedChunk = mergeChunks chunk1 chunk2
           in mergeSmallChunks minSize (mergedChunk : rest)
        else chunk1 : mergeSmallChunks minSize (chunk2 : rest)

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
splitLargeChunks maxSize = concatMap (splitIfTooLarge maxSize)

splitIfTooLarge :: Int -> TextChunk -> [TextChunk]
splitIfTooLarge maxSize chunk =
  let chunkTxt = chunkContent chunk
      contentLength = T.length chunkTxt
   in if contentLength <= maxSize
        then [chunk]
        else splitChunkBySize maxSize chunk

splitChunkBySize :: Int -> TextChunk -> [TextChunk]
splitChunkBySize maxSize chunk =
  let textLines = T.lines (chunkContent chunk)
      lineCount = length textLines
      avgLineLength = if lineCount > 0 then T.length (chunkContent chunk) `div` lineCount else 0
      estimatedLinesPerChunk = if avgLineLength > 0 then maxSize `div` avgLineLength else 10
      chunkedLines = chunksOf (max 1 estimatedLinesPerChunk) textLines
   in map (createChunkFromTextLines (chunkStartLine chunk)) chunkedLines

createChunkFromTextLines :: Int -> [Text] -> TextChunk
createChunkFromTextLines startLineOffset textLines =
  let combinedContent = T.unlines textLines
      newChunkId = createChunkId combinedContent
      lineCount = length textLines
      endLine = startLineOffset + lineCount - 1
      -- インデントレベルは最初の行から推定
      computedIndentLevel = if null textLines then 0 else T.length $ T.takeWhile (\c -> c == ' ' || c == '\t') (head textLines)
   in TextChunk newChunkId combinedContent startLineOffset endLine computedIndentLevel

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
detectChunkBoundaries = concatMap detectLineBoundary
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
extractReferences _ =
  -- 実際の実装では、リンクや参照構文を解析
  -- ここでは簡単な例として空リストを返す
  []