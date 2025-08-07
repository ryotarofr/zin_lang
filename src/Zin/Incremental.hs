{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Zin.Incremental
  ( ChunkId (..),
    TextChunk (..),
    CompiledChunk (..),
    CompilerState (..),
    Change (..),
    CacheEntry (..),
    IncrementalResult (..),
    createChunkId,
    emptyCompilerState,
    isChunkValid,
    invalidateChunk,
    updateChunk,
  )
where

import qualified Crypto.Hash.SHA256 as SHA256
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Zin.AST (Block)
import Zin.VirtualDOM (VNode)

-- チャンクの一意識別子（コンテンツハッシュベース）
newtype ChunkId = ChunkId Text
  deriving (Show, Eq, Ord, Generic)

-- テキストチャンク（論理的な処理単位）
data TextChunk = TextChunk
  { chunkId :: ChunkId,
    chunkContent :: Text,
    chunkStartLine :: Int,
    chunkEndLine :: Int,
    chunkIndentLevel :: Int
  }
  deriving (Show, Eq, Generic)

-- コンパイル済みチャンク
data CompiledChunk = CompiledChunk
  { compiledChunkId :: ChunkId,
    compiledBlocks :: [Block],
    compiledVNodes :: [VNode],
    compiledHTML :: Text,
    lastCompiled :: UTCTime,
    dependencies :: [ChunkId]
  }
  deriving (Show, Eq, Generic)

-- 変更の種類
data Change
  = ChunkAdded ChunkId TextChunk
  | ChunkRemoved ChunkId
  | ChunkModified ChunkId TextChunk TextChunk -- old, new
  | ChunkMoved ChunkId Int Int -- chunkId, oldPosition, newPosition
  deriving (Show, Eq, Generic)

-- キャッシュエントリ
data CacheEntry = CacheEntry
  { cacheChunk :: CompiledChunk,
    cacheHash :: Text,
    cacheTimestamp :: UTCTime,
    cacheSize :: Int
  }
  deriving (Show, Eq, Generic)

-- 増分コンパイルの結果
data IncrementalResult = IncrementalResult
  { incrementalHTML :: Text,
    compiledChunks :: [ChunkId],
    reusedChunks :: [ChunkId],
    totalTime :: Double,
    cacheHitRate :: Double
  }
  deriving (Show, Eq, Generic)

-- コンパイラの状態
data CompilerState = CompilerState
  { stateChunks :: Map ChunkId CompiledChunk,
    stateCache :: Map ChunkId CacheEntry,
    stateDependencies :: Map ChunkId [ChunkId],
    stateInverseDependencies :: Map ChunkId [ChunkId],
    stateLastModified :: UTCTime,
    stateConfig :: CompilerConfig
  }
  deriving (Show, Eq, Generic)

-- コンパイラ設定
data CompilerConfig = CompilerConfig
  { maxCacheSize :: Int,
    cacheTimeout :: Int, -- seconds
    enableChunkMerging :: Bool,
    enableDependencyTracking :: Bool,
    chunkSizeThreshold :: Int -- minimum chunk size
  }
  deriving (Show, Eq, Generic)

defaultCompilerConfig :: CompilerConfig
defaultCompilerConfig =
  CompilerConfig
    { maxCacheSize = 1000,
      cacheTimeout = 3600, -- 1 hour
      enableChunkMerging = True,
      enableDependencyTracking = True,
      chunkSizeThreshold = 50 -- minimum 50 characters
    }

-- チャンクIDを生成（コンテンツハッシュベース）
createChunkId :: Text -> ChunkId
createChunkId content =
  let hash = SHA256.hash (encodeUtf8 content)
      hashText = T.pack $ show hash
   in ChunkId $ T.take 16 hashText -- 16文字のハッシュプレフィックス

-- 空のコンパイラ状態を作成
emptyCompilerState :: IO CompilerState
emptyCompilerState = do
  now <- getCurrentTime
  return
    CompilerState
      { stateChunks = Map.empty,
        stateCache = Map.empty,
        stateDependencies = Map.empty,
        stateInverseDependencies = Map.empty,
        stateLastModified = now,
        stateConfig = defaultCompilerConfig
      }

-- チャンクが有効かどうかチェック
isChunkValid :: ChunkId -> CompilerState -> Bool
isChunkValid chunkId state =
  case Map.lookup chunkId (stateChunks state) of
    Nothing -> False
    Just chunk ->
      let deps = Map.findWithDefault [] chunkId (stateDependencies state)
          depsValid = all (`Map.member` stateChunks state) deps
       in depsValid

-- チャンクを無効化（依存チェーンも含む）
invalidateChunk :: ChunkId -> CompilerState -> CompilerState
invalidateChunk chunkId state =
  let -- 直接・間接的に依存しているチャンクを取得
      dependents = getAllDependents chunkId state
      -- チャンクとその依存者をキャッシュから削除
      newChunks = foldr Map.delete (stateChunks state) (chunkId : dependents)
      newCache = foldr Map.delete (stateCache state) (chunkId : dependents)
   in state
        { stateChunks = newChunks,
          stateCache = newCache
        }

-- すべての依存者を再帰的に取得
getAllDependents :: ChunkId -> CompilerState -> [ChunkId]
getAllDependents chunkId state =
  let directDependents = Map.findWithDefault [] chunkId (stateInverseDependencies state)
      indirectDependents = concatMap (`getAllDependents` state) directDependents
   in directDependents ++ indirectDependents

-- チャンクを更新
updateChunk :: ChunkId -> CompiledChunk -> CompilerState -> IO CompilerState
updateChunk chunkId compiledChunk state = do
  now <- getCurrentTime
  let newState =
        state
          { stateChunks = Map.insert chunkId compiledChunk (stateChunks state),
            stateLastModified = now
          }
  return $ updateDependencies chunkId (dependencies compiledChunk) newState

-- 依存関係を更新
updateDependencies :: ChunkId -> [ChunkId] -> CompilerState -> CompilerState
updateDependencies chunkId deps state =
  let -- 古い依存関係を削除
      oldDeps = Map.findWithDefault [] chunkId (stateDependencies state)
      stateWithoutOldInverse = foldr (removeInverseDependency chunkId) state oldDeps

      -- 新しい依存関係を追加
      newStateDeps = Map.insert chunkId deps (stateDependencies stateWithoutOldInverse)
      newStateInverse = foldr (addInverseDependency chunkId) stateWithoutOldInverse deps
   in newStateInverse {stateDependencies = newStateDeps}

-- 逆依存関係を追加
addInverseDependency :: ChunkId -> ChunkId -> CompilerState -> CompilerState
addInverseDependency dependent dependency state =
  let currentInverse = Map.findWithDefault [] dependency (stateInverseDependencies state)
      newInverse =
        if dependent `elem` currentInverse
          then currentInverse
          else dependent : currentInverse
   in state {stateInverseDependencies = Map.insert dependency newInverse (stateInverseDependencies state)}

-- 逆依存関係を削除
removeInverseDependency :: ChunkId -> ChunkId -> CompilerState -> CompilerState
removeInverseDependency dependent dependency state =
  let currentInverse = Map.findWithDefault [] dependency (stateInverseDependencies state)
      newInverse = filter (/= dependent) currentInverse
      newInverseMap =
        if null newInverse
          then Map.delete dependency (stateInverseDependencies state)
          else Map.insert dependency newInverse (stateInverseDependencies state)
   in state {stateInverseDependencies = newInverseMap}

-- キャッシュサイズ制限の実装
limitCacheSize :: CompilerState -> CompilerState
limitCacheSize state =
  let config = stateConfig state
      currentSize = Map.size (stateCache state)
      maxSize = maxCacheSize config
   in if currentSize <= maxSize
        then state
        else
          let -- タイムスタンプでソートして古いエントリから削除
              sortedEntries = Map.toList (stateCache state)
              sortedByTime =
                take maxSize $
                  map fst $
                    sortOn (cacheTimestamp . snd) sortedEntries
              newCache =
                Map.fromList $
                  filter (\(k, _) -> k `elem` sortedByTime) sortedEntries
           in state {stateCache = newCache}

-- デバッグ用の状態情報取得
getStateInfo :: CompilerState -> (Int, Int, Int, Double)
getStateInfo state =
  let chunkCount = Map.size (stateChunks state)
      cacheCount = Map.size (stateCache state)
      depCount = Map.size (stateDependencies state)
      avgDeps =
        if chunkCount == 0
          then 0.0
          else fromIntegral (sum $ map length $ Map.elems (stateDependencies state)) / fromIntegral chunkCount
   in (chunkCount, cacheCount, depCount, avgDeps)