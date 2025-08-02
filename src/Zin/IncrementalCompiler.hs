{-# LANGUAGE OverloadedStrings #-}

module Zin.IncrementalCompiler 
  ( IncrementalCompiler(..)
  , CompilationMode(..)
  , CompilationResult(..)
  , CompilationPerformance(..)
  , compileIncremental
  , initializeCompiler
  , loadCompilerState
  , saveCompilerState
  , recompileChanged
  , analyzePerformance
  , getCompilerStats
  , optimizeCompiler
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad (foldM)
import Control.Exception (try, SomeException)

import Zin.Incremental
import Zin.Chunking
import Zin.Diff
import Zin.Cache (CacheManager(..), CachePolicy(..), CacheKey(..), createCacheManager, defaultCachePolicy, getCacheStats, getCacheSize, lookupCache, insertCache, optimizeCache)
import Zin.AST
import Zin.Parser
import Zin.Semantic
import Zin.VirtualCodeGen
import Zin.VirtualDOM (VNode(..))
import Zin.Renderer
import Zin.Error

-- コンパイルモード
data CompilationMode 
  = FullCompilation      -- 全体コンパイル
  | IncrementalOnly      -- 増分のみ
  | SmartIncremental     -- スマート増分（依存関係追跡）
  | FastIncremental      -- 高速増分（最小限のチェック）
  deriving (Show, Eq)

-- 増分コンパイラの状態
data IncrementalCompiler = IncrementalCompiler
  { compilerState :: CompilerState
  , cacheManager :: CacheManager
  , lastSource :: Maybe Text
  , lastChunks :: [TextChunk]
  , compilationMode :: CompilationMode
  , performanceHistory :: [CompilationPerformance]
  } deriving (Show, Eq)

-- コンパイル性能情報
data CompilationPerformance = CompilationPerformance
  { perfTimestamp :: UTCTime
  , perfTotalTime :: Double
  , perfChunksProcessed :: Int
  , perfChunksReused :: Int
  , perfCacheHitRate :: Double
  , perfMemoryUsage :: Int
  , perfMode :: CompilationMode
  } deriving (Show, Eq)

-- コンパイル結果
data CompilationResult = CompilationResult
  { resultHTML :: Text
  , resultChunksCompiled :: [ChunkId]
  , resultChunksReused :: [ChunkId]
  , resultPerformance :: CompilationPerformance
  , resultWarnings :: [Text]
  , resultErrors :: [ParseError]
  } deriving (Show, Eq)

-- コンパイラの初期化
initializeCompiler :: CompilationMode -> IO IncrementalCompiler
initializeCompiler mode = do
  state <- emptyCompilerState
  cache <- createCacheManager defaultCachePolicy
  return IncrementalCompiler
    { compilerState = state
    , cacheManager = cache
    , lastSource = Nothing
    , lastChunks = []
    , compilationMode = mode
    , performanceHistory = []
    }

-- メインの増分コンパイル関数
compileIncremental :: MonadIO m => Text -> IncrementalCompiler -> m (Either [ParseError] CompilationResult, IncrementalCompiler)
compileIncremental source compiler = liftIO $ do
  startTime <- getCurrentTime
  
  case lastSource compiler of
    Nothing -> do
      -- 初回コンパイル
      result <- performFullCompilation source compiler startTime
      return result
    Just lastSrc -> do
      -- 増分コンパイル
      case compilationMode compiler of
        FullCompilation -> performFullCompilation source compiler startTime
        IncrementalOnly -> performIncrementalCompilation lastSrc source compiler startTime
        SmartIncremental -> performSmartIncremental lastSrc source compiler startTime
        FastIncremental -> performFastIncremental lastSrc source compiler startTime

-- 全体コンパイル
performFullCompilation :: Text -> IncrementalCompiler -> UTCTime -> IO (Either [ParseError] CompilationResult, IncrementalCompiler)
performFullCompilation source compiler startTime = do
  -- チャンク分割
  let chunks = chunkifyText ByTopLevelBlocks source
  
  -- 各チャンクをコンパイル
  (compiledChunks, newState, newCache, errors) <- compileChunks chunks (compilerState compiler) (cacheManager compiler)
  
  endTime <- getCurrentTime
  let totalTime = realToFrac $ diffUTCTime endTime startTime
  
  case errors of
    [] -> do
      -- HTMLの結合
      let htmlParts = map compiledHTML compiledChunks
      let finalHTML = T.concat htmlParts
      
      -- 性能情報の記録
      perf <- createPerformanceRecord startTime endTime (compilationMode compiler) 
                (length chunks) 0 0 (getCacheSize newCache)
      
      let result = CompilationResult
            { resultHTML = finalHTML
            , resultChunksCompiled = map compiledChunkId compiledChunks
            , resultChunksReused = []
            , resultPerformance = perf
            , resultWarnings = []
            , resultErrors = []
            }
      
      let newCompiler = compiler
            { compilerState = newState
            , cacheManager = newCache
            , lastSource = Just source
            , lastChunks = chunks
            , performanceHistory = perf : take 100 (performanceHistory compiler)
            }
      
      return (Right result, newCompiler)
    _ -> do
      let result = CompilationResult
            { resultHTML = ""
            , resultChunksCompiled = []
            , resultChunksReused = []
            , resultPerformance = CompilationPerformance startTime 0 0 0 0 0 (compilationMode compiler)
            , resultWarnings = []
            , resultErrors = errors
            }
      return (Left errors, compiler)

-- 増分コンパイル
performIncrementalCompilation :: Text -> Text -> IncrementalCompiler -> UTCTime -> IO (Either [ParseError] CompilationResult, IncrementalCompiler)
performIncrementalCompilation oldSource newSource compiler startTime = do
  -- 差分計算
  let diffResult = diffTexts oldSource newSource
  let changes = diffChanges diffResult
  
  -- 変更されたチャンクを特定
  let newChunks = chunkifyText ByTopLevelBlocks newSource
  let changedChunks = identifyChangedChunks (lastChunks compiler) newChunks changes
  
  -- 変更されたチャンクのみ再コンパイル
  (compiledNew, reusedExisting, newState, newCache, errors) <- 
    recompileChangedChunks changedChunks (lastChunks compiler) newChunks 
                          (compilerState compiler) (cacheManager compiler)
  
  endTime <- getCurrentTime
  let totalTime = realToFrac $ diffUTCTime endTime startTime
  
  case errors of
    [] -> do
      -- HTMLの結合
      let allCompiled = compiledNew ++ reusedExisting
      let htmlParts = map compiledHTML allCompiled
      let finalHTML = T.concat htmlParts
      
      -- 性能情報の記録
      perf <- createPerformanceRecord startTime endTime (compilationMode compiler)
                (length changedChunks) (length reusedExisting) 
                (length reusedExisting) (getCacheSize newCache)
      
      let result = CompilationResult
            { resultHTML = finalHTML
            , resultChunksCompiled = map compiledChunkId compiledNew
            , resultChunksReused = map compiledChunkId reusedExisting
            , resultPerformance = perf
            , resultWarnings = []
            , resultErrors = []
            }
      
      let newCompiler = compiler
            { compilerState = newState
            , cacheManager = newCache
            , lastSource = Just newSource
            , lastChunks = newChunks
            , performanceHistory = perf : take 100 (performanceHistory compiler)
            }
      
      return (Right result, newCompiler)
    _ -> return (Left errors, compiler)

-- スマート増分コンパイル（依存関係を考慮）
performSmartIncremental :: Text -> Text -> IncrementalCompiler -> UTCTime -> IO (Either [ParseError] CompilationResult, IncrementalCompiler)
performSmartIncremental oldSource newSource compiler startTime = do
  -- 基本的な増分コンパイルを実行
  (result, newCompiler) <- performIncrementalCompilation oldSource newSource compiler startTime
  
  case result of
    Left errors -> return (Left errors, newCompiler)
    Right compResult -> do
      -- 依存関係を再計算して追加の無効化を実行
      let newState = analyzeDependencies (lastChunks newCompiler) (compilerState newCompiler)
      let finalCompiler = newCompiler { compilerState = newState }
      return (Right compResult, finalCompiler)

-- 高速増分コンパイル（最小限のチェック）
performFastIncremental :: Text -> Text -> IncrementalCompiler -> UTCTime -> IO (Either [ParseError] CompilationResult, IncrementalCompiler)
performFastIncremental oldSource newSource compiler startTime = do
  -- 簡易的な差分検出（ハッシュベース）
  let oldHash = T.take 32 $ T.pack $ show $ hash oldSource
  let newHash = T.take 32 $ T.pack $ show $ hash newSource
  
  if oldHash == newHash
    then do
      -- 変更なし：キャッシュされた結果を返す
      endTime <- getCurrentTime
      let totalTime = realToFrac $ diffUTCTime endTime startTime
      
      perf <- createPerformanceRecord startTime endTime FastIncremental 0 
                (length $ lastChunks compiler) (length $ lastChunks compiler) 
                (getCacheSize $ cacheManager compiler)
      
      let result = CompilationResult
            { resultHTML = "" -- 前回の結果を使用
            , resultChunksCompiled = []
            , resultChunksReused = map chunkId (lastChunks compiler)
            , resultPerformance = perf
            , resultWarnings = []
            , resultErrors = []
            }
      
      return (Right result, compiler { performanceHistory = perf : performanceHistory compiler })
    else
      -- 変更あり：通常の増分コンパイルを実行
      performIncrementalCompilation oldSource newSource compiler startTime

-- 複数チャンクのコンパイル
compileChunks :: [TextChunk] -> CompilerState -> CacheManager -> IO ([CompiledChunk], CompilerState, CacheManager, [ParseError])
compileChunks chunks state cache = do
  (compiled, newState, newCache, errors) <- foldM compileChunkWithState ([], state, cache, []) chunks
  return (reverse compiled, newState, newCache, errors)
  where
    compileChunkWithState (acc, s, c, errs) chunk = do
      (maybeCompiled, newS, newC, newErrs) <- compileSingleChunk chunk s c
      case maybeCompiled of
        Nothing -> return (acc, newS, newC, errs ++ newErrs)
        Just compiled -> return (compiled : acc, newS, newC, errs ++ newErrs)

-- 単一チャンクのコンパイル
compileSingleChunk :: TextChunk -> CompilerState -> CacheManager -> IO (Maybe CompiledChunk, CompilerState, CacheManager, [ParseError])
compileSingleChunk chunk state cache = do
  let key = CacheKey $ let ChunkId cid = chunkId chunk in cid
  
  -- キャッシュチェック
  (cached, newCache) <- lookupCache key cache
  case cached of
    Just compiledChunk -> do
      -- キャッシュヒット
      return (Just compiledChunk, state, newCache, [])
    Nothing -> do
      -- 新規コンパイル
      result <- compileChunkFromScratch chunk
      case result of
        Left err -> return (Nothing, state, newCache, [err])
        Right compiledChunk -> do
          -- キャッシュに保存
          finalCache <- insertCache key compiledChunk newCache
          newState <- updateChunk (chunkId chunk) compiledChunk state
          return (Just compiledChunk, newState, finalCache, [])

-- チャンクを一からコンパイル
compileChunkFromScratch :: TextChunk -> IO (Either ParseError CompiledChunk)
compileChunkFromScratch chunk = do
  now <- getCurrentTime
  let content = chunkContent chunk
  
  case parseZinFile content of
    Left parseErr -> return $ Left parseErr
    Right document -> do
      case semanticAnalysis document of
        Left semanticErr -> return $ Left semanticErr
        Right validatedDoc -> do
          let blocks = case validatedDoc of
                Document bs -> bs
          let vdom = VText "placeholder"  -- TODO: implement generateVDOMFromDocument
          let html = "placeholder"  -- TODO: implement proper HTML rendering
          let vNodes = [vdom]
          
          let compiledChunk = CompiledChunk
                { compiledChunkId = chunkId chunk
                , compiledBlocks = blocks
                , compiledVNodes = vNodes
                , compiledHTML = html
                , lastCompiled = now
                , dependencies = []  -- 簡易実装
                }
          
          return $ Right compiledChunk

-- 変更されたチャンクの特定
identifyChangedChunks :: [TextChunk] -> [TextChunk] -> [Change] -> [TextChunk]
identifyChangedChunks oldChunks newChunks changes = 
  let changedIds = extractChangedIds changes
      oldMap = Map.fromList $ map (\c -> (chunkId c, c)) oldChunks
      newMap = Map.fromList $ map (\c -> (chunkId c, c)) newChunks
      
      -- 新しいチャンクまたは変更されたチャンクを特定
      changedInNew = filter (\c -> chunkId c `elem` changedIds || 
                                   chunkId c `Map.notMember` oldMap) newChunks
  in changedInNew

extractChangedIds :: [Change] -> [ChunkId]
extractChangedIds changes = concatMap extractIds changes
  where
    extractIds (ChunkAdded cid _) = [cid]
    extractIds (ChunkRemoved cid) = [cid]
    extractIds (ChunkModified cid _ _) = [cid]
    extractIds (ChunkMoved cid _ _) = [cid]

-- 変更されたチャンクの再コンパイル
recompileChangedChunks :: [TextChunk] -> [TextChunk] -> [TextChunk] -> CompilerState -> CacheManager 
                      -> IO ([CompiledChunk], [CompiledChunk], CompilerState, CacheManager, [ParseError])
recompileChangedChunks changedChunks oldChunks newChunks state cache = do
  -- 変更されたチャンクを再コンパイル
  (recompiled, newState1, newCache1, errors1) <- compileChunks changedChunks state cache
  
  -- 変更されていないチャンクをキャッシュから取得
  let unchangedChunks = filter (\c -> chunkId c `notElem` map chunkId changedChunks) newChunks
  (reused, newState2, newCache2, errors2) <- getReuseableChunks unchangedChunks newState1 newCache1
  
  return (recompiled, reused, newState2, newCache2, errors1 ++ errors2)

-- 再利用可能なチャンクの取得
getReuseableChunks :: [TextChunk] -> CompilerState -> CacheManager -> IO ([CompiledChunk], CompilerState, CacheManager, [ParseError])
getReuseableChunks chunks state cache = do
  (reused, newCache, errors) <- foldM getReuseableChunk ([], cache, []) chunks
  return (reverse reused, state, newCache, errors)
  where
    getReuseableChunk (acc, c, errs) chunk = do
      let key = CacheKey $ let ChunkId cid = chunkId chunk in cid
      (cached, newC) <- lookupCache key c
      case cached of
        Just compiledChunk -> return (compiledChunk : acc, newC, errs)
        Nothing -> do
          -- キャッシュにない場合は再コンパイル
          result <- compileChunkFromScratch chunk
          case result of
            Left err -> return (acc, newC, err : errs)
            Right compiledChunk -> do
              finalC <- insertCache key compiledChunk newC
              return (compiledChunk : acc, finalC, errs)

-- 性能レコードの作成
createPerformanceRecord :: UTCTime -> UTCTime -> CompilationMode -> Int -> Int -> Int -> (Int, Int) -> IO CompilationPerformance
createPerformanceRecord startTime endTime mode processed reused hitCount (entries, memory) = do
  let totalTime = realToFrac $ diffUTCTime endTime startTime
  let hitRate = if processed + reused > 0 
                then fromIntegral hitCount / fromIntegral (processed + reused)
                else 0.0
  return CompilationPerformance
    { perfTimestamp = startTime
    , perfTotalTime = totalTime
    , perfChunksProcessed = processed
    , perfChunksReused = reused
    , perfCacheHitRate = hitRate
    , perfMemoryUsage = memory
    , perfMode = mode
    }

-- 依存関係の分析
analyzeDependencies :: [TextChunk] -> CompilerState -> CompilerState
analyzeDependencies chunks state = 
  let dependencies = calculateChunkDependencies chunks
      newStateDeps = Map.fromList dependencies
  in state { stateDependencies = newStateDeps }

-- コンパイラ状態の保存
saveCompilerState :: FilePath -> IncrementalCompiler -> IO (Either String ())
saveCompilerState filepath compiler = do
  result <- try $ do
    -- 簡易実装：将来的にはJSONシリアライゼーションを実装
    writeFile filepath $ show compiler
    return ()
  case result of
    Left ex -> return $ Left $ "Failed to save state: " ++ show (ex :: SomeException)
    Right _ -> return $ Right ()

-- コンパイラ状態の読み込み
loadCompilerState :: FilePath -> IO (Maybe IncrementalCompiler)
loadCompilerState filepath = do
  result <- (try $ readFile filepath) :: IO (Either SomeException String)
  case result of
    Left _ -> return Nothing
    Right _ -> do
      -- TODO: Implement proper deserialization
      return Nothing

-- 性能分析
analyzePerformance :: IncrementalCompiler -> IO Text
analyzePerformance compiler = do
  let history = performanceHistory compiler
  if null history
    then return "No performance data available."
    else do
      let avgTime = sum (map perfTotalTime history) / fromIntegral (length history)
      let avgHitRate = sum (map perfCacheHitRate history) / fromIntegral (length history)
      let totalProcessed = sum (map perfChunksProcessed history)
      let totalReused = sum (map perfChunksReused history)
      
      return $ T.concat
        [ "Performance Analysis:\n"
        , "Average compilation time: ", T.pack $ show avgTime, " seconds\n"
        , "Average cache hit rate: ", T.pack $ show (avgHitRate * 100), "%\n"
        , "Total chunks processed: ", T.pack $ show totalProcessed, "\n"
        , "Total chunks reused: ", T.pack $ show totalReused, "\n"
        , "Compilation sessions: ", T.pack $ show (length history), "\n"
        ]

-- コンパイラ統計の取得
getCompilerStats :: IncrementalCompiler -> (Int, Int, Double, Int)  -- (chunks, cache_entries, hit_rate, sessions)
getCompilerStats compiler = 
  let chunkCount = length (lastChunks compiler)
      (cacheEntries, _) = getCacheSize (cacheManager compiler)
      hitRate = 0.0  -- TODO: implement proper hit rate calculation
      sessionCount = length (performanceHistory compiler)
  in (chunkCount, cacheEntries, hitRate, sessionCount)

-- コンパイラの最適化
optimizeCompiler :: MonadIO m => IncrementalCompiler -> m IncrementalCompiler
optimizeCompiler compiler = liftIO $ do
  -- キャッシュの最適化
  optimizedCache <- optimizeCache (cacheManager compiler)
  
  -- 性能履歴の制限
  let limitedHistory = take 50 (performanceHistory compiler)
  
  return compiler 
    { cacheManager = optimizedCache
    , performanceHistory = limitedHistory
    }

-- 簡易ハッシュ関数
hash :: Text -> Int
hash = T.foldl' (\acc c -> acc * 31 + fromEnum c) 0

-- 変更されたチャンクのみ再コンパイル（公開API）
recompileChanged :: MonadIO m => Text -> Text -> IncrementalCompiler -> m (Either [ParseError] CompilationResult, IncrementalCompiler)
recompileChanged oldSource newSource = compileIncremental newSource . setLastSource oldSource
  where
    setLastSource src compiler = compiler { lastSource = Just src }