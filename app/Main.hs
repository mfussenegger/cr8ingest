{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Cli
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (Value (..), decodeStrict)
import qualified Data.ByteString.Char8  as BS
import           Data.Function          ((&))
import           Data.HashMap.Strict    ((!))
import           Data.Maybe             (fromJust, isJust)
import           Data.Text              (Text)
import qualified Data.Vector            as V
import qualified Db
import           Hasql.Pool             (Pool, use)
import qualified Hasql.Session          as HS
import           Hasql.Statement        (Statement)
import           Prelude                hiding (head, tail)
import qualified Streamly               as S
import qualified Streamly.Prelude       as S
import           System.Clock           (Clock (..), TimeSpec (..), getTime)
import qualified System.IO              as IO
import           Text.Printf            (printf)



type Records a = V.Vector (V.Vector a)


isObject :: Value -> Bool
isObject (Object _) = True
isObject _          = False


extractValues :: Functor f => f Text -> Value -> f Value
extractValues columns obj = getValue obj <$> columns
  where
    getValue (Object o) column = o ! column
    getValue _ _ = error "extractValues has to be used with objects"


executeInsert :: Pool
              -> Statement (Records a) b
              -> Records a
              -> IO Double
executeInsert pool insert values = do
  start <- getTime Monotonic
  result <- use pool session
  end <- getTime Monotonic
  case result of
    Left err -> error $ show err
    Right _ -> pure $ fromIntegral (nsec (end - start)) / 1000.0 / 1000.0
  where
    session = HS.statement values insert


chunksOf :: Monad m => Int -> S.SerialT m a -> S.SerialT m (V.Vector a)
chunksOf chunkSize = consume chunkSize V.empty
  where
    consume 0 items stream = S.yield items <>
                             consume chunkSize V.empty stream
    consume n items stream = do
      parts <- S.yieldM $ S.uncons stream
      case parts of
        Nothing           -> S.yield items
        Just (head, tail) -> consume (n - 1) (V.cons head items) tail


-- | Convert a vector of rows to column store representation
-- >>> columnStore $ V.fromList <$> (V.fromList [[1, 10], [2, 20]])
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
  & fmap decodeStrict
  & S.filter isJust
  & fmap fromJust
  & S.filter isObject
  & fmap (extractValues (fst <$> columns))


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
  & chunksOf bulkSize
  & fmap columnStore
  & S.filter (not . V.null)


data RuntimeStats = RuntimeStats
  { start :: TimeSpec
  , opCount :: Integer
  , opDuration :: Double }


main :: IO ()
main = do
  args <- Cli.getArgs
  let
    dbUri = Cli.dbUri args
    table = Cli.table args
    rate = Cli.avgRate args
    concurrency = Cli.concurrency args
    bulkSize = Cli.bulkSize args
  _ <- Db.withPool concurrency dbUri (ingest table concurrency bulkSize rate)
  putChar '\n'
  putStrLn "done"
  where
    ingest tableName concurrency bulkSize rate pool = do
      insertCtx <- Db.createInsertContext tableName pool
      let
        columns = Db.tableColumns insertCtx
        setRate = maybe id S.avgRate rate
        records = S.serially $ getRecords columns bulkSize
        insert = executeInsert pool (Db.insertStatement insertCtx)
      print columns
      print $ "rate: " <> show rate
      print $ "concurrency: " <> show concurrency
      IO.hSetBuffering IO.stdout IO.NoBuffering
      start <- getTime Monotonic
      S.foldlM' reportProgress RuntimeStats { start = start, opCount = 0, opDuration = 0 }
        $ records
        & S.asyncly . setRate . S.maxThreads concurrency . S.mapM insert
      where
        reportProgress stats@RuntimeStats{..} durationInMs = do
          now <- sec <$> getTime Monotonic
          let
            newOpCount = opCount + 1
            newTotalDuration = opDuration + durationInMs
            ops = (fromIntegral newOpCount :: Double) / fromIntegral (now - sec start)
            avgDuration = newTotalDuration / fromIntegral newOpCount
          putStr $ printf "op/s: %.2f  avg duration: %.3f\r" ops avgDuration
          pure stats { opCount = newOpCount, opDuration = newTotalDuration }
