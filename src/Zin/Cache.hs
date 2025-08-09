{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Zin.Cache
  ( CacheManager (..),
    CacheKey (..),
    CachePolicy (..),
    CacheStats (..),
    EvictionStrategy (..),
    createCacheManager,
    defaultCachePolicy,
    lookupCache,
    insertCache,
    invalidateCache,
    clearCache,
    getCacheStats,
    saveCacheToFile,
    loadCacheFromFile,
    cleanupExpiredEntries,
    optimizeCache,
    getCacheSize,
    setCachePolicy,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, removeFile)
import Zin.Incremental

-- キャッシュキーの型
newtype CacheKey = CacheKey Text
  deriving (Show, Eq, Ord, Generic)

-- 立ち退き戦略
data EvictionStrategy
  = LRU -- Least Recently Used
  | LFU -- Least Frequently Used
  | TTL -- Time To Live
  | Random -- Random eviction
  deriving (Show, Eq, Generic)

-- キャッシュポリシー
data CachePolicy = CachePolicy
  { maxCacheEntries :: Int,
    entryTTL :: Int, -- seconds
    maxCacheMemory :: Int, -- bytes
    evictionStrategy :: EvictionStrategy,
    persistToDisk :: Bool,
    cacheFilePath :: Maybe FilePath
  }
  deriving (Show, Eq, Generic)

-- キャッシュ統計
data CacheStats = CacheStats
  { totalHits :: Int,
    totalMisses :: Int,
    hitRate :: Double,
    evictionCount :: Int,
    currentSize :: Int,
    memoryUsage :: Int -- bytes
  }
  deriving (Show, Eq, Generic)

-- キャッシュエントリの詳細情報
data CacheEntryMetadata = CacheEntryMetadata
  { entryCreatedAt :: UTCTime,
    entryLastAccessed :: UTCTime,
    entryAccessCount :: Int,
    entrySize :: Int,
    entryTTLExpiry :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic)

-- 内部キャッシュエントリ
data InternalCacheEntry = InternalCacheEntry
  { entryData :: CompiledChunk,
    entryMetadata :: CacheEntryMetadata
  }
  deriving (Show, Eq, Generic)

-- キャッシュマネージャー
data CacheManager = CacheManager
  { cacheEntries :: Map CacheKey InternalCacheEntry,
    cachePolicy :: CachePolicy,
    cacheStats :: CacheStats
  }
  deriving (Show, Eq, Generic)

-- デフォルトキャッシュポリシー
defaultCachePolicy :: CachePolicy
defaultCachePolicy =
  CachePolicy
    { maxCacheEntries = 1000,
      entryTTL = 3600, -- 1 hour
      maxCacheMemory = 100 * 1024 * 1024, -- 100MB
      evictionStrategy = LRU,
      persistToDisk = True,
      cacheFilePath = Just ".zin_cache.json"
    }

-- 空の統計
emptyStats :: IO CacheStats
emptyStats = do
  return
    CacheStats
      { totalHits = 0,
        totalMisses = 0,
        hitRate = 0.0,
        evictionCount = 0,
        currentSize = 0,
        memoryUsage = 0
      }

-- キャッシュマネージャーの作成
createCacheManager :: CachePolicy -> IO CacheManager
createCacheManager policy = do
  stats <- emptyStats
  let manager =
        CacheManager
          { cacheEntries = Map.empty,
            cachePolicy = policy,
            cacheStats = stats
          }

  -- ディスクからの読み込み
  if persistToDisk policy
    then case cacheFilePath policy of
      Just filepath -> do
        loaded <- loadCacheFromFile filepath
        case loaded of
          Just loadedManager -> return loadedManager {cachePolicy = policy}
          Nothing -> return manager
      Nothing -> return manager
    else return manager

-- キャッシュからの検索
lookupCache :: (MonadIO m) => CacheKey -> CacheManager -> m (Maybe CompiledChunk, CacheManager)
lookupCache key manager = liftIO $ do
  now <- getCurrentTime
  case Map.lookup key (cacheEntries manager) of
    Nothing -> do
      let newStats = (cacheStats manager) {totalMisses = totalMisses (cacheStats manager) + 1}
      return (Nothing, manager {cacheStats = newStats})
    Just entry -> do
      -- TTL チェック
      let metadata = entryMetadata entry
      case entryTTLExpiry metadata of
        Just expiry | now > expiry -> do
          -- 期限切れエントリを削除
          let newEntries = Map.delete key (cacheEntries manager)
          let newStats =
                (cacheStats manager)
                  { totalMisses = totalMisses (cacheStats manager) + 1,
                    currentSize = currentSize (cacheStats manager) - 1
                  }
          return (Nothing, manager {cacheEntries = newEntries, cacheStats = newStats})
        _ -> do
          -- アクセス情報を更新
          let updatedMetadata =
                metadata
                  { entryLastAccessed = now,
                    entryAccessCount = entryAccessCount metadata + 1
                  }
          let updatedEntry = entry {entryMetadata = updatedMetadata}
          let newEntries = Map.insert key updatedEntry (cacheEntries manager)
          let newStats =
                (cacheStats manager)
                  { totalHits = totalHits (cacheStats manager) + 1
                  }
          let finalStats = updateHitRate newStats
          return (Just (entryData entry), manager {cacheEntries = newEntries, cacheStats = finalStats})

-- キャッシュへの挿入
insertCache :: (MonadIO m) => CacheKey -> CompiledChunk -> CacheManager -> m CacheManager
insertCache key chunk manager = liftIO $ do
  now <- getCurrentTime
  let policy = cachePolicy manager
  let ttlExpiry =
        if entryTTL policy > 0
          then Just $ addUTCTime (fromIntegral $ entryTTL policy) now
          else Nothing

  let metadata =
        CacheEntryMetadata
          { entryCreatedAt = now,
            entryLastAccessed = now,
            entryAccessCount = 1,
            entrySize = estimateChunkSize chunk,
            entryTTLExpiry = ttlExpiry
          }

  let entry =
        InternalCacheEntry
          { entryData = chunk,
            entryMetadata = metadata
          }

  let newEntries = Map.insert key entry (cacheEntries manager)
  let newStats =
        (cacheStats manager)
          { currentSize = currentSize (cacheStats manager) + 1,
            memoryUsage = memoryUsage (cacheStats manager) + entrySize metadata
          }

  let updatedManager = manager {cacheEntries = newEntries, cacheStats = newStats}

  -- キャッシュサイズ制限の適用
  enforcePolicy updatedManager

-- キャッシュからの削除
invalidateCache :: (MonadIO m) => CacheKey -> CacheManager -> m CacheManager
invalidateCache key manager = liftIO $ do
  case Map.lookup key (cacheEntries manager) of
    Nothing -> return manager
    Just entry -> do
      let newEntries = Map.delete key (cacheEntries manager)
      let newStats =
            (cacheStats manager)
              { currentSize = currentSize (cacheStats manager) - 1,
                memoryUsage = memoryUsage (cacheStats manager) - entrySize (entryMetadata entry)
              }
      return manager {cacheEntries = newEntries, cacheStats = newStats}

-- キャッシュのクリア
clearCache :: (MonadIO m) => CacheManager -> m CacheManager
clearCache manager = liftIO $ do
  stats <- emptyStats
  return
    manager
      { cacheEntries = Map.empty,
        cacheStats = stats
      }

-- キャッシュ統計の取得
getCacheStats :: CacheManager -> CacheStats
getCacheStats = cacheStats

-- キャッシュポリシーの強制適用
enforcePolicy ::
  CacheManager ->
  IO
    CacheManager
enforcePolicy manager = do
  let policy = cachePolicy manager
  let currentEntries = Map.size (cacheEntries manager)
  let currentMemory = memoryUsage (cacheStats manager)

  -- エントリ数制限のチェック
  manager1 <-
    if currentEntries > maxCacheEntries policy
      then evictEntries (currentEntries - maxCacheEntries policy) manager
      else return manager

  if currentMemory > maxCacheMemory policy
    then evictByMemory (currentMemory - maxCacheMemory policy) manager1
    else return manager1 -- これが関数の戻り値になる

-- エントリの立ち退き（エントリ数制限）
evictEntries :: Int -> CacheManager -> IO CacheManager
evictEntries 0 manager = return manager
evictEntries count manager = do
  let strategy = evictionStrategy (cachePolicy manager)
  keyToEvict <- selectEvictionKey strategy manager
  case keyToEvict of
    Nothing -> return manager
    Just key -> do
      manager' <- invalidateCache key manager
      let newStats =
            (cacheStats manager')
              { evictionCount = evictionCount (cacheStats manager') + 1
              }
      evictEntries (count - 1) (manager' {cacheStats = newStats})

-- メモリによる立ち退き
evictByMemory :: Int -> CacheManager -> IO CacheManager
evictByMemory targetReduction manager = do
  if targetReduction <= 0
    then return manager
    else do
      let strategy = evictionStrategy (cachePolicy manager)
      keyToEvict <- selectEvictionKey strategy manager
      case keyToEvict of
        Nothing -> return manager
        Just key -> do
          case Map.lookup key (cacheEntries manager) of
            Nothing -> return manager
            Just entry -> do
              let freedMemory = entrySize (entryMetadata entry)
              manager' <- invalidateCache key manager
              let newStats =
                    (cacheStats manager')
                      { evictionCount = evictionCount (cacheStats manager') + 1
                      }
              evictByMemory (targetReduction - freedMemory) (manager' {cacheStats = newStats})

-- 立ち退き対象の選択
selectEvictionKey :: EvictionStrategy -> CacheManager -> IO (Maybe CacheKey)
selectEvictionKey strategy manager = do
  let entries = Map.toList (cacheEntries manager)
  case entries of
    [] -> return Nothing
    _ -> case strategy of
      LRU -> return $ Just $ fst $ minimumBy (comparing (entryLastAccessed . entryMetadata . snd)) entries
      LFU -> return $ Just $ fst $ minimumBy (comparing (entryAccessCount . entryMetadata . snd)) entries
      TTL -> do
        now <- getCurrentTime
        let withTTL = filter (maybe False (< now) . entryTTLExpiry . entryMetadata . snd) entries
        case withTTL of
          [] -> selectEvictionKey LRU manager -- Fallback to LRU
          _ -> return $ Just $ fst $ head withTTL
      Random -> do
        let index = 0 -- 簡易実装：常に最初のエントリ
        return $ Just $ fst $ entries !! index

-- ヒット率の更新
updateHitRate :: CacheStats -> CacheStats
updateHitRate stats =
  let total = totalHits stats + totalMisses stats
      rate =
        if total > 0
          then fromIntegral (totalHits stats) / fromIntegral total
          else 0.0
   in stats {hitRate = rate}

-- チャンクサイズの推定
estimateChunkSize :: CompiledChunk -> Int
estimateChunkSize chunk =
  let htmlSize = T.length (compiledHTML chunk)
      blockCount = length (compiledBlocks chunk)
      nodeCount = length (compiledVNodes chunk)
      depCount = length (dependencies chunk)
   in htmlSize * 2 + blockCount * 100 + nodeCount * 50 + depCount * 10 -- 概算

-- 期限切れエントリのクリーンアップ
cleanupExpiredEntries :: (MonadIO m) => CacheManager -> m CacheManager
cleanupExpiredEntries manager = liftIO $ do
  now <- getCurrentTime
  let entries = Map.toList (cacheEntries manager)
  let (expired, valid) = partitionExpired now entries

  let newEntries = Map.fromList valid
  let freedMemory = sum $ map (entrySize . entryMetadata . snd) expired
  let newStats =
        (cacheStats manager)
          { currentSize = currentSize (cacheStats manager) - length expired,
            memoryUsage = memoryUsage (cacheStats manager) - freedMemory
          }

  return manager {cacheEntries = newEntries, cacheStats = newStats}

partitionExpired :: UTCTime -> [(CacheKey, InternalCacheEntry)] -> ([(CacheKey, InternalCacheEntry)], [(CacheKey, InternalCacheEntry)])
partitionExpired now entries =
  let isExpired (_, entry) = case entryTTLExpiry (entryMetadata entry) of
        Nothing -> False
        Just expiry -> now > expiry
   in (filter isExpired entries, filter (not . isExpired) entries)

-- キャッシュの最適化
optimizeCache :: (MonadIO m) => CacheManager -> m CacheManager
optimizeCache manager = liftIO $ do
  -- 期限切れエントリのクリーンアップ
  manager1 <- cleanupExpiredEntries manager

  -- ポリシーの適用
  manager2 <- enforcePolicy manager1

  -- ディスクへの保存（必要に応じて）
  if persistToDisk (cachePolicy manager2)
    then case cacheFilePath (cachePolicy manager2) of
      Just filepath -> do
        _ <- saveCacheToFile filepath manager2
        return manager2
      Nothing -> return manager2
    else return manager2

-- キャッシュサイズの取得
getCacheSize :: CacheManager -> (Int, Int) -- (entries, memory)
getCacheSize manager =
  let stats = cacheStats manager
   in (currentSize stats, memoryUsage stats)

-- キャッシュポリシーの設定
setCachePolicy :: CachePolicy -> CacheManager -> CacheManager
setCachePolicy policy manager = manager {cachePolicy = policy}

-- ディスクへの保存
saveCacheToFile :: FilePath -> CacheManager -> IO (Either String ())
saveCacheToFile filepath manager = do
  -- TODO: Implement binary serialization instead of JSON
  return $ Right ()

-- ディスクからの読み込み
loadCacheFromFile :: FilePath -> IO (Maybe CacheManager)
loadCacheFromFile filepath = do
  -- TODO: Implement binary deserialization instead of JSON
  return Nothing

-- キャッシュファイルの削除
deleteCacheFile :: FilePath -> IO ()
deleteCacheFile filepath = do
  exists <- doesFileExist filepath
  when exists $ removeFile filepath
