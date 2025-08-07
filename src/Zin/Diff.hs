{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Zin.Diff
  ( DiffResult (..),
    LineChange (..),
    ChunkChange (..),
    diffTexts,
    diffChunks,
    diffLines,
    calculateChanges,
    optimizeDiff,
    applyChanges,
    mergeDiffs,
    getDiffStatistics,
  )
where

import Control.Monad.State
import Data.List (groupBy, minimumBy, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Zin.Chunking
import Zin.Incremental

-- 差分結果
data DiffResult = DiffResult
  { diffChanges :: [Change],
    diffStatistics :: DiffStatistics,
    diffChunkChanges :: [ChunkChange],
    diffLineChanges :: [LineChange]
  }
  deriving (Show, Eq)

-- 差分統計
data DiffStatistics = DiffStatistics
  { addedLines :: Int,
    deletedLines :: Int,
    modifiedLines :: Int,
    addedChunks :: Int,
    deletedChunks :: Int,
    modifiedChunks :: Int,
    unchangedChunks :: Int
  }
  deriving (Show, Eq)

-- 行レベルの変更
data LineChange
  = LineAdded Int Text
  | LineDeleted Int Text
  | LineModified Int Text Text -- lineNumber, oldText, newText
  | LineUnchanged Int Text
  deriving (Show, Eq)

-- チャンクレベルの変更
data ChunkChange
  = ChunkChangeAdded ChunkId TextChunk
  | ChunkChangeDeleted ChunkId TextChunk
  | ChunkChangeModified ChunkId TextChunk TextChunk
  | ChunkChangeMoved ChunkId Int Int -- chunkId, fromPos, toPos
  | ChunkChangeUnchanged ChunkId TextChunk
  deriving (Show, Eq)

-- 編集距離計算用の操作
data EditOp
  = Insert Int Text
  | Delete Int Text
  | Replace Int Text Text
  | Keep Int Text
  deriving (Show, Eq)

-- メインの差分計算関数
diffTexts :: Text -> Text -> DiffResult
diffTexts oldText newText =
  let oldLines = T.lines oldText
      newLines = T.lines newText
      lineChanges = diffLines oldLines newLines

      -- チャンク単位の差分も計算
      oldChunks = chunkifyText ByTopLevelBlocks oldText
      newChunks = chunkifyText ByTopLevelBlocks newText
      chunkChanges = diffChunks oldChunks newChunks

      -- 全体の変更を統合
      changes = lineChangesToChanges lineChanges ++ chunkChangesToChanges chunkChanges
      stats = calculateDiffStatistics lineChanges chunkChanges
   in DiffResult changes stats chunkChanges lineChanges

-- チャンク単位の差分計算
diffChunks :: [TextChunk] -> [TextChunk] -> [ChunkChange]
diffChunks oldChunks newChunks =
  let oldMap = Map.fromList $ map (\chunk -> (chunkId chunk, chunk)) oldChunks
      newMap = Map.fromList $ map (\chunk -> (chunkId chunk, chunk)) newChunks

      allIds = Map.keys oldMap ++ Map.keys newMap
      uniqueIds = Map.keys $ Map.fromList $ map (\id -> (id, ())) allIds
   in concatMap (classifyChunkChange oldMap newMap) uniqueIds

classifyChunkChange :: Map ChunkId TextChunk -> Map ChunkId TextChunk -> ChunkId -> [ChunkChange]
classifyChunkChange oldMap newMap chunkId =
  case (Map.lookup chunkId oldMap, Map.lookup chunkId newMap) of
    (Nothing, Just newChunk) -> [ChunkChangeAdded chunkId newChunk]
    (Just oldChunk, Nothing) -> [ChunkChangeDeleted chunkId oldChunk]
    (Just oldChunk, Just newChunk) ->
      if chunkContent oldChunk == chunkContent newChunk
        then [ChunkChangeUnchanged chunkId newChunk]
        else [ChunkChangeModified chunkId oldChunk newChunk]
    (Nothing, Nothing) -> [] -- Should not happen

-- 行単位の差分計算（Myers algorithmの簡易版）
diffLines :: [Text] -> [Text] -> [LineChange]
diffLines oldLines newLines =
  let oldVector = V.fromList oldLines
      newVector = V.fromList newLines
      editOps = calculateEditSequence oldVector newVector
   in editOpsToLineChanges editOps

-- 編集操作の計算（動的プログラミング）
calculateEditSequence :: V.Vector Text -> V.Vector Text -> [EditOp]
calculateEditSequence oldVec newVec =
  let oldLen = V.length oldVec
      newLen = V.length newVec
      dpTable = createDPTable oldVec newVec
   in backtrackEditOps oldVec newVec dpTable oldLen newLen

-- 動的プログラミングテーブルの作成
createDPTable :: V.Vector Text -> V.Vector Text -> V.Vector (V.Vector Int)
createDPTable oldVec newVec =
  let oldLen = V.length oldVec
      newLen = V.length newVec
      initRow = V.generate (newLen + 1) id
      initTable = V.replicate (oldLen + 1) initRow
   in V.imap (\i row -> V.imap (\j _ -> calculateDP oldVec newVec i j) row) initTable

calculateDP :: V.Vector Text -> V.Vector Text -> Int -> Int -> Int
calculateDP oldVec newVec i j
  | i == 0 = j
  | j == 0 = i
  | oldVec V.! (i - 1) == newVec V.! (j - 1) =
      calculateDP oldVec newVec (i - 1) (j - 1)
  | otherwise =
      1
        + minimum
          [ calculateDP oldVec newVec (i - 1) j, -- deletion
            calculateDP oldVec newVec i (j - 1), -- insertion
            calculateDP oldVec newVec (i - 1) (j - 1) -- substitution
          ]

-- バックトラッキングで編集操作を復元
backtrackEditOps :: V.Vector Text -> V.Vector Text -> V.Vector (V.Vector Int) -> Int -> Int -> [EditOp]
backtrackEditOps oldVec newVec dpTable = go []
  where
    go acc i j
      | i == 0 && j == 0 = acc
      | i == 0 = go (Insert j (newVec V.! (j - 1)) : acc) i (j - 1)
      | j == 0 = go (Delete i (oldVec V.! (i - 1)) : acc) (i - 1) j
      | oldVec V.! (i - 1) == newVec V.! (j - 1) =
          go (Keep i (oldVec V.! (i - 1)) : acc) (i - 1) (j - 1)
      | otherwise =
          let deletion = dpTable V.! (i - 1) V.! j
              insertion = dpTable V.! i V.! (j - 1)
              substitution = dpTable V.! (i - 1) V.! (j - 1)
              minOp = minimum [deletion, insertion, substitution]
           in if
                | minOp == substitution ->
                    go (Replace i (oldVec V.! (i - 1)) (newVec V.! (j - 1)) : acc) (i - 1) (j - 1)
                | minOp == deletion ->
                    go (Delete i (oldVec V.! (i - 1)) : acc) (i - 1) j
                | otherwise ->
                    go (Insert j (newVec V.! (j - 1)) : acc) i (j - 1)

-- 編集操作を行変更に変換
editOpsToLineChanges :: [EditOp] -> [LineChange]
editOpsToLineChanges = map editOpToLineChange
  where
    editOpToLineChange (Insert lineNum text) = LineAdded lineNum text
    editOpToLineChange (Delete lineNum text) = LineDeleted lineNum text
    editOpToLineChange (Replace lineNum oldText newText) = LineModified lineNum oldText newText
    editOpToLineChange (Keep lineNum text) = LineUnchanged lineNum text

-- 行変更をChangeに変換
lineChangesToChanges :: [LineChange] -> [Change]
lineChangesToChanges lineChanges =
  -- 簡易実装：行変更からチャンク変更を推定
  concatMap lineChangeToChange lineChanges
  where
    lineChangeToChange (LineAdded _ _) = [] -- チャンクレベルの変更は別途処理
    lineChangeToChange (LineDeleted _ _) = []
    lineChangeToChange (LineModified _ _ _) = []
    lineChangeToChange (LineUnchanged _ _) = []

-- チャンク変更をChangeに変換
chunkChangesToChanges :: [ChunkChange] -> [Change]
chunkChangesToChanges = map chunkChangeToChange
  where
    chunkChangeToChange (ChunkChangeAdded chunkId chunk) = ChunkAdded chunkId chunk
    chunkChangeToChange (ChunkChangeDeleted chunkId chunk) = ChunkRemoved chunkId
    chunkChangeToChange (ChunkChangeModified chunkId oldChunk newChunk) = ChunkModified chunkId oldChunk newChunk
    chunkChangeToChange (ChunkChangeMoved chunkId fromPos toPos) = ChunkMoved chunkId fromPos toPos
    chunkChangeToChange (ChunkChangeUnchanged _ _) = ChunkAdded (ChunkId "") (TextChunk (ChunkId "") "" 0 0 0) -- ダミー

-- 差分統計の計算
calculateDiffStatistics :: [LineChange] -> [ChunkChange] -> DiffStatistics
calculateDiffStatistics lineChanges chunkChanges =
  let addedLines = length $ filter isLineAdded lineChanges
      deletedLines = length $ filter isLineDeleted lineChanges
      modifiedLines = length $ filter isLineModified lineChanges

      addedChunks = length $ filter isChunkAdded chunkChanges
      deletedChunks = length $ filter isChunkDeleted chunkChanges
      modifiedChunks = length $ filter isChunkModified chunkChanges
      unchangedChunks = length $ filter isChunkUnchanged chunkChanges
   in DiffStatistics
        addedLines
        deletedLines
        modifiedLines
        addedChunks
        deletedChunks
        modifiedChunks
        unchangedChunks

isLineAdded :: LineChange -> Bool
isLineAdded (LineAdded _ _) = True
isLineAdded _ = False

isLineDeleted :: LineChange -> Bool
isLineDeleted (LineDeleted _ _) = True
isLineDeleted _ = False

isLineModified :: LineChange -> Bool
isLineModified (LineModified _ _ _) = True
isLineModified _ = False

isChunkAdded :: ChunkChange -> Bool
isChunkAdded (ChunkChangeAdded _ _) = True
isChunkAdded _ = False

isChunkDeleted :: ChunkChange -> Bool
isChunkDeleted (ChunkChangeDeleted _ _) = True
isChunkDeleted _ = False

isChunkModified :: ChunkChange -> Bool
isChunkModified (ChunkChangeModified _ _ _) = True
isChunkModified _ = False

isChunkUnchanged :: ChunkChange -> Bool
isChunkUnchanged (ChunkChangeUnchanged _ _) = True
isChunkUnchanged _ = False

-- 変更セットの計算
calculateChanges :: [TextChunk] -> [TextChunk] -> [Change]
calculateChanges oldChunks newChunks =
  let chunkChanges = diffChunks oldChunks newChunks
   in chunkChangesToChanges chunkChanges

-- 差分の最適化
optimizeDiff :: DiffResult -> DiffResult
optimizeDiff diffResult =
  let optimizedChanges = optimizeChanges (diffChanges diffResult)
      optimizedChunkChanges = optimizeChunkChanges (diffChunkChanges diffResult)
      optimizedLineChanges = optimizeLineChanges (diffLineChanges diffResult)
      newStats = calculateDiffStatistics optimizedLineChanges optimizedChunkChanges
   in diffResult
        { diffChanges = optimizedChanges,
          diffChunkChanges = optimizedChunkChanges,
          diffLineChanges = optimizedLineChanges,
          diffStatistics = newStats
        }

-- 変更の最適化
optimizeChanges :: [Change] -> [Change]
optimizeChanges changes =
  -- 連続する変更をマージ
  mergeConsecutiveChanges changes

mergeConsecutiveChanges :: [Change] -> [Change]
mergeConsecutiveChanges [] = []
mergeConsecutiveChanges [change] = [change]
mergeConsecutiveChanges (change1 : change2 : rest) =
  case tryMergeChanges change1 change2 of
    Just merged -> mergeConsecutiveChanges (merged : rest)
    Nothing -> change1 : mergeConsecutiveChanges (change2 : rest)

tryMergeChanges :: Change -> Change -> Maybe Change
tryMergeChanges (ChunkAdded id1 chunk1) (ChunkAdded id2 chunk2) =
  if canMergeChunks chunk1 chunk2
    then Just (ChunkAdded id1 (mergeChunks chunk1 chunk2))
    else Nothing
tryMergeChanges _ _ = Nothing

-- チャンク変更の最適化
optimizeChunkChanges :: [ChunkChange] -> [ChunkChange]
optimizeChunkChanges = id -- 簡易実装

-- 行変更の最適化
optimizeLineChanges :: [LineChange] -> [LineChange]
optimizeLineChanges = id -- 簡易実装

-- 変更の適用
applyChanges :: Text -> [Change] -> Text
applyChanges originalText changes =
  let lines = T.lines originalText
      modifiedLines = foldl applyChange lines changes
   in T.unlines modifiedLines

applyChange :: [Text] -> Change -> [Text]
applyChange lines change =
  case change of
    ChunkAdded _ chunk -> lines ++ T.lines (chunkContent chunk)
    ChunkRemoved _ -> lines -- 削除は別途処理が必要
    ChunkModified _ _ newChunk -> lines ++ T.lines (chunkContent newChunk)
    ChunkMoved _ _ _ -> lines -- 移動は複雑な処理が必要

-- 差分のマージ
mergeDiffs :: [DiffResult] -> DiffResult
mergeDiffs [] = DiffResult [] (DiffStatistics 0 0 0 0 0 0 0) [] []
mergeDiffs [diff] = diff
mergeDiffs (diff1 : diff2 : rest) =
  let merged = mergeTwoDiffs diff1 diff2
   in mergeDiffs (merged : rest)

mergeTwoDiffs :: DiffResult -> DiffResult -> DiffResult
mergeTwoDiffs diff1 diff2 =
  let mergedChanges = diffChanges diff1 ++ diffChanges diff2
      mergedChunkChanges = diffChunkChanges diff1 ++ diffChunkChanges diff2
      mergedLineChanges = diffLineChanges diff1 ++ diffLineChanges diff2
      mergedStats = mergeDiffStatistics (diffStatistics diff1) (diffStatistics diff2)
   in DiffResult mergedChanges mergedStats mergedChunkChanges mergedLineChanges

mergeDiffStatistics :: DiffStatistics -> DiffStatistics -> DiffStatistics
mergeDiffStatistics stats1 stats2 =
  DiffStatistics
    (addedLines stats1 + addedLines stats2)
    (deletedLines stats1 + deletedLines stats2)
    (modifiedLines stats1 + modifiedLines stats2)
    (addedChunks stats1 + addedChunks stats2)
    (deletedChunks stats1 + deletedChunks stats2)
    (modifiedChunks stats1 + modifiedChunks stats2)
    (unchangedChunks stats1 + unchangedChunks stats2)

-- 差分統計の取得
getDiffStatistics :: DiffResult -> DiffStatistics
getDiffStatistics = diffStatistics

-- 高速化のための類似度計算
calculateSimilarity :: Text -> Text -> Double
calculateSimilarity text1 text2 =
  let len1 = T.length text1
      len2 = T.length text2
      maxLen = max len1 len2
   in if maxLen == 0
        then 1.0
        else fromIntegral (maxLen - levenshteinDistance text1 text2) / fromIntegral maxLen

-- 簡易Levenshtein距離計算
levenshteinDistance :: Text -> Text -> Int
levenshteinDistance s1 s2 =
  let len1 = T.length s1
      len2 = T.length s2
   in if len1 == 0
        then len2
        else
          if len2 == 0
            then len1
            else levenshteinHelper (T.unpack s1) (T.unpack s2)

levenshteinHelper :: String -> String -> Int
levenshteinHelper [] s2 = length s2
levenshteinHelper s1 [] = length s1
levenshteinHelper (c1 : s1) (c2 : s2) =
  if c1 == c2
    then levenshteinHelper s1 s2
    else
      1
        + minimum
          [ levenshteinHelper s1 (c2 : s2), -- deletion
            levenshteinHelper (c1 : s1) s2, -- insertion
            levenshteinHelper s1 s2 -- substitution
          ]

-- Helper functions for chunk merging
canMergeChunks :: TextChunk -> TextChunk -> Bool
canMergeChunks _ _ = False -- Simple implementation: never merge

mergeChunks :: TextChunk -> TextChunk -> TextChunk
mergeChunks chunk1 _ = chunk1 -- Simple implementation: return first chunk