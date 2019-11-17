{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Strict #-}

module Main where

import qualified Cli
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), decodeStrict')
import qualified Data.ByteString.Char8 as BS
import Data.Function ((&))
import Data.HashMap.Strict ((!))
import Data.Maybe (fromJust, isJust)
import qualified Data.Pool as P
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Db
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Word (Word64)
import qualified Hasql.Session as HS
import Hasql.Statement (Statement)
import qualified Streamly as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Prelude as S
import qualified System.IO as IO
import Text.Printf (printf)

-- $setup
-- >>> :set -XOverloadedLists


type Records a = V.Vector (V.Vector a)


executeInsert :: Db.Pool
              -> Statement (Records a) b
              -> Records a
              -> IO Double
executeInsert pool insert values = do
  start <- getMonotonicTimeNSec
  result <- P.withResource pool (HS.run session)
  end <- getMonotonicTimeNSec
  let
    durationInNs = end - start
    durationInMs = nsToMs durationInNs
  case result of
    Left err -> error $ show err
    Right _ -> pure durationInMs
  where
    session = HS.statement values insert



-- | Convert a vector of rows to column store representation
-- >>> columnStore [[1, 10], [2, 20]]
-- [[1,2],[10,20]]
columnStore :: Records a -> Records a
columnStore = V.foldr' stepper V.empty
  where
    stepper row newList
      | V.null newList = fmap V.singleton row
      | otherwise      = V.zipWith V.cons row newList


parseInput :: (S.IsStream t, Monad m, Functor f, Functor (t m))
           => f (Text, b)
           -> t m BS.ByteString
           -> t m (f Value)
parseInput columns input = input
  & fmap decodeStrict'
  & S.filter isJust
  & fmap fromJust
  & S.filter isObject
  & fmap (extractValues (fst <$> columns))
  where
    extractValues :: Functor f => f Text -> Value -> f Value
    extractValues columnNames obj = getValue obj <$> columnNames
      where
        getValue (Object o) columnName = o ! columnName
        getValue _ _ = error "extractValues has to be used with objects"
    isObject :: Value -> Bool
    isObject (Object _) = True
    isObject _          = False


fromStdin :: S.SerialT IO BS.ByteString
fromStdin = do
  eof <- liftIO IO.isEOF
  if eof
    then S.nil
    else S.yieldM BS.getLine <> fromStdin


getRecords :: V.Vector (Text, Text) -> Int -> S.SerialT IO (Records Value)
getRecords columns bulkSize =
  fromStdin
  & parseInput columns
  & S.chunksOf bulkSize FL.toList
  & fmap V.fromList
  & fmap columnStore
  & S.filter (not . V.null)


data RuntimeStats = RuntimeStats
  { startInMs :: Double
  , opTotalCount :: Integer
  , opTotalDurationInMs :: Double 
  , lastUpdate :: Double }


mkStats :: RuntimeStats
mkStats = RuntimeStats
  { startInMs = 0
  , opTotalCount = 0
  , opTotalDurationInMs = 0 
  , lastUpdate = 0} 


nsToMs :: Word64 -> Double
nsToMs ns = fromIntegral ns / (1000.0 * 1000.0)


main :: IO ()
main = do
  args <- Cli.getArgs
  let
    dbUris = Cli.dbUri args
    table = Cli.table args
    rate = Cli.avgRate args
    concurrency = Cli.concurrency args
    bulkSize = Cli.bulkSize args
  _ <- Db.withPool concurrency dbUris (ingest table concurrency bulkSize rate)
  putChar '\n'
  where
    ingest tableName concurrency bulkSize rate pool = do
      insertCtx <- Db.createInsertContext tableName pool
      let
        columns = Db.tableColumns insertCtx
        throttle = case rate of
          Nothing    -> S.maxThreads concurrency
          Just rate' -> S.avgRate rate'
        records = S.serially $ getRecords columns bulkSize
        insert = executeInsert pool (Db.insertStatement insertCtx)
      putStrLn $ "Columns: " <> show columns
      putStrLn $ "Rate: " <> show rate
      putStrLn $ "Concurrency: " <> show concurrency
      start <- getMonotonicTimeNSec
      IO.hSetBuffering IO.stdout IO.NoBuffering
      S.foldlM' reportProgress mkStats { startInMs = nsToMs start }
        $ records
        & S.asyncly . throttle . S.mapM insert
      where
        reportProgress RuntimeStats{startInMs, opTotalCount, opTotalDurationInMs, lastUpdate} durationInMs = do
          now <- nsToMs <$> getMonotonicTimeNSec
          let
            newOpCount = opTotalCount + 1
            opCount :: Double
            opCount = fromIntegral newOpCount
            elapsedInS = (now - startInMs) / 1000.0
            avgDurationInMs = opTotalDurationInMs / opCount
            operationsPerSec = opCount / elapsedInS
            newLastUpdate = if now - lastUpdate > 50 then now else lastUpdate
          when (now - lastUpdate > 50) $
            putStr $ printf "%d requests [op/s: %.2f  avg duration: %.3f (ms)]\r" newOpCount operationsPerSec avgDurationInMs
          pure RuntimeStats
            { startInMs = startInMs
            , opTotalCount = newOpCount
            , opTotalDurationInMs = opTotalDurationInMs + durationInMs 
            , lastUpdate = newLastUpdate }
