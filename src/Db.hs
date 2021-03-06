
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Db
  ( Pool
  , withPool
  , createInsertContext
  , InsertContext(..)
  )
where

import           Control.Exception          (finally)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Aeson                 as A
import           Data.Functor.Contravariant (Contravariant, contramap)
import           Data.Maybe                 (fromJust)
import qualified Data.Pool                  as P
import qualified Data.Scientific            as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Time.Clock.POSIX      (POSIXTime, posixSecondsToUTCTime)
import           Data.Time.Format           (defaultTimeLocale,
                                             iso8601DateFormat,
                                             parseTimeOrError)
import           Data.Vector                (Vector, (!))
import qualified Data.Vector                as V
import qualified GHC.Int
import qualified Hasql.Connection           as HC
import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import qualified Hasql.Session              as HS
import           Hasql.Statement            (Statement (..))
import           System.Random              (randomRIO)
import           Text.RawString.QQ          (r)

-- $setup
-- >>> :set -XOverloadedStrings


type Pool = P.Pool HC.Connection
type DbSession a = ReaderT Pool IO a

newtype RelationName = RelationName (Text, Text)
                       deriving (Show)


withPool :: Int -> [Text] -> (Pool -> IO a) -> IO a
withPool poolSize dbUris f = do
  pool <- P.createPool createConn HC.release poolSize 50 poolSize
  f pool `finally` P.destroyAllResources pool
  where
    createConn = do
      idx <- randomRIO (0, length dbUris - 1)
      let dbUri = dbUris !! idx
      errOrConnection <- HC.acquire (T.encodeUtf8 dbUri)
      case errOrConnection of
        Left err   -> error $ show err
        Right conn -> pure conn


-- | Parse fully qualified table name into schema and table
--
-- >>> parseTableName "defaultSchemaName" "foo.bar"
-- RelationName ("foo","bar")
--
-- >>> parseTableName "doc" "bar"
-- RelationName ("doc","bar")
--
-- >>> parseTableName "doc" ""
-- RelationName ("doc","")
parseTableName :: Text -> Text -> RelationName
parseTableName defaultSchema tableName =
    RelationName $ case parts of
      []     -> (defaultSchema, "")
      [x]    -> (defaultSchema, x)
      (x:xs) -> (x, T.intercalate "" xs)
  where
    parts = T.splitOn "." tableName


getColumns :: RelationName -> DbSession (Vector (Text, Text))
getColumns (RelationName (schema, table)) = run session
  where
    session = HS.statement (schema, table) selectColumns


selectColumns :: Statement (Text, Text) (Vector (Text, Text))
selectColumns = Statement sql encoder decoder False
  where
    sql = [r|SELECT
  column_name,
  data_type
FROM 
  information_schema.columns
WHERE 
  cast(is_generated as string) in ('f', 'NEVER')
  AND table_schema = $1
  AND table_name = $2
  AND column_name NOT LIKE '%]'
ORDER BY 
  ordinal_position ASC|]
    encoder =
      contramap fst (HE.param (HE.nonNullable HE.text)) <>
      contramap snd (HE.param (HE.nonNullable HE.text))
    row = (,) <$> HD.column (HD.nonNullable HD.text) <*> HD.column (HD.nonNullable HD.text)
    decoder =
      HD.rowVector row


run :: HS.Session a -> DbSession a
run session = do
  pool <- Reader.ask
  result <- liftIO $ P.withResource pool (HS.run session)
  case result of
    Left err   -> error $ show err
    Right rows -> pure rows


getCurrentSchema :: DbSession Text
getCurrentSchema = run session
  where
    session = HS.statement () selectSchema
    selectSchema = Statement sql HE.noParams (HD.singleRow (HD.column (HD.nonNullable HD.text))) False
    sql = "SELECT current_schema()"


insert :: RelationName -> Vector (Text, Text) -> Statement (Vector (Vector A.Value)) GHC.Int.Int64
insert (RelationName (schema, table)) columns =
  Statement sql encoders decoders True
  where
    sql = T.encodeUtf8 $ T.unlines
      [ "INSERT INTO "
      , fqTable
      , " ("
      , targetColumns
      , ") SELECT * FROM UNNEST("
      , params
      , ")"
      ]
      where
        fqTable = schema <> "." <> table
        columnNames = fst <$> columns
        joinByComma = T.intercalate ", " . V.toList
        targetColumns = joinByComma columnNames
        numberedColumns :: Vector (Int, Text)
        numberedColumns = V.zip [1..] columnNames
        params = joinByComma $ ("$" <>) . T.pack . show . fst <$> numberedColumns
    encoders = encodersForColumns columns
    decoders = HD.rowsAffected


encoderForNumber :: Contravariant f
                 => Text
                 -> (S.Scientific -> b)
                 -> f b
                 -> f A.Value
encoderForNumber typeName castNumber = contramap extractNumber
  where
    extractNumber (A.Number x) = castNumber x
    extractNumber x = error $ "Expected " <> T.unpack typeName <> ", got: " <> show x


getText :: A.Value -> Text
getText (A.String x) = x
getText x            = error $ "Expected string, got: " <> show x


valueEncoderForType :: Text -> HE.Value A.Value
valueEncoderForType t@"long" = encoderForNumber t (fromJust . S.toBoundedInteger) HE.int8
valueEncoderForType t@"bigint" = encoderForNumber t (fromJust . S.toBoundedInteger) HE.int8
valueEncoderForType t@"integer" = encoderForNumber t (fromJust . S.toBoundedInteger) HE.int4
valueEncoderForType t@"short" = encoderForNumber t (fromJust . S.toBoundedInteger) HE.int2
valueEncoderForType t@"smallint" = encoderForNumber t (fromJust . S.toBoundedInteger) HE.int2
valueEncoderForType t@"byte" = encoderForNumber t (fromJust . S.toBoundedInteger) HE.int2
valueEncoderForType t@"char" = encoderForNumber t (fromJust . S.toBoundedInteger) HE.int2
valueEncoderForType t@"float" = encoderForNumber t S.toRealFloat HE.float4
valueEncoderForType t@"real" = encoderForNumber t S.toRealFloat HE.float4
valueEncoderForType t@"double" = encoderForNumber t S.toRealFloat HE.float8
valueEncoderForType t@"double precision" = encoderForNumber t S.toRealFloat HE.float8
valueEncoderForType "object" = contramap id HE.json
valueEncoderForType "string" = contramap getText HE.text
valueEncoderForType "text" = contramap getText HE.text
valueEncoderForType "timestamp" = contramap parseTime HE.timestamptz
  where
    parseTime (A.String x) = parseTimeOrError False defaultTimeLocale isoFormat (T.unpack x)
    parseTime (A.Number x) = fromSeconds x
    parseTime x = error $ "Expected a timestamp, got: " <> show x
    isoFormat = iso8601DateFormat (Just "%H:%M:%SZ")
    secondsToPosixTime :: S.Scientific -> POSIXTime
    secondsToPosixTime x = (fromInteger . truncate . (* 1000) $ (S.toRealFloat x :: Double)) / 1000
    fromSeconds = posixSecondsToUTCTime . secondsToPosixTime
valueEncoderForType typeName =
  if T.isSuffixOf "_array" typeName
    then contramap getArr HE.json
    else error $ "Encoder for type: " <> T.unpack typeName <> " not implemented"
  where
    getArr a@(A.Array _) = a
    getArr x = error $ "Expected array, got: " <> show x


encodersForColumns :: Vector (Text, Text) -> HE.Params (Vector (Vector A.Value))
encodersForColumns columns = mconcat $ V.toList params
  where
    params = asParam <$> V.zip [0..] (fmap snd columns)
    asParam (idx, dataType) = contramap (! idx) (vector $ HE.nonNullable $ valueEncoderForType dataType)
    vector value =
      HE.param $ HE.nonNullable (HE.array (HE.dimension V.foldl (HE.element value)))


data InsertContext = InsertContext
  { insertStatement :: Statement (Vector (Vector A.Value)) GHC.Int.Int64
  , tableColumns :: Vector (Text, Text) }


createInsertContext :: Text -> Pool -> IO InsertContext
createInsertContext table = runReaderT f
  where
    f = do
      currentSchema <- getCurrentSchema
      let relationName = parseTableName currentSchema table
      columns <- getColumns relationName
      let insertStmt = insert relationName columns
      pure $ InsertContext insertStmt columns
