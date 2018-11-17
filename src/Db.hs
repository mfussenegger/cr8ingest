
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Db
  ( withPool
  , createInsertContext
  , InsertContext(..)
  )
where

import           Control.Exception          (finally)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Aeson                 as A
import           Data.Functor.Contravariant (contramap)
import           Data.Maybe                 (fromJust)
import qualified Data.Scientific            as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Vector                (Vector, (!))
import qualified Data.Vector                as V
import qualified GHC.Int
import qualified Hasql.Decoders             as HD
import qualified Hasql.Encoders             as HE
import           Hasql.Pool                 (Pool, acquire, release, use)
import qualified Hasql.Session              as HS
import           Hasql.Statement            (Statement (..))
import           Text.RawString.QQ          (r)

-- $setup
-- >>> :set -XOverloadedStrings


type DbSession a = ReaderT Pool IO a

newtype RelationName = RelationName (Text, Text)
                       deriving (Show)


withPool :: Int -> Text -> (Pool -> IO a) -> IO a
withPool poolSize dbUri f = do
  pool <- acquire (poolSize, 500000, T.encodeUtf8 dbUri)
  f pool `finally` release pool


-- | Parse fully qualified table name into schema and table
--
-- >>> parseTableName "defaultSchemaName" "foo.bar"
-- RelationName ("foo","bar")
--
-- >>> parseTableName "doc" "bar"
-- RelationName ("doc","bar")
parseTableName :: Text -> Text -> RelationName
parseTableName defaultSchema tableName =
    RelationName $ case parts of
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
  is_generated = false
  AND table_schema = $1
  AND table_name = $2
  AND column_name NOT LIKE '%]'
ORDER BY 
  ordinal_position ASC|]
    encoder =
      contramap fst (HE.param HE.text) <>
      contramap snd (HE.param HE.text)
    row = (,) <$> HD.column HD.text <*> HD.column HD.text
    decoder =
      HD.rowVector row


run :: HS.Session a -> DbSession a
run session = do
  pool <- Reader.ask
  result <- liftIO $ use pool session
  case result of
    Left err -> error $ show err
    Right rows -> pure rows


getCurrentSchema :: DbSession Text
getCurrentSchema = run session
  where
    session = HS.statement () selectSchema
    selectSchema = Statement sql HE.unit (HD.singleRow (HD.column HD.text)) False
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
        numberedColumns = V.zip (V.fromList [1..]) columnNames
        params = joinByComma $ ("$" <>) . T.pack . show . fst <$> numberedColumns
    encoders = encodersForColumns columns
    decoders = HD.rowsAffected


valueEncoderForType :: Text -> HE.Value A.Value
valueEncoderForType "integer" = contramap getInt HE.int4
  where
    getInt (A.Number x) = fromJust $ S.toBoundedInteger x
valueEncoderForType "string" = contramap getText HE.text
  where
    getText (A.String x) = x
    getText x = error $ "Expected string, got: " <> show x


encodersForColumns :: Vector (Text, Text) -> HE.Params (Vector (Vector A.Value))
encodersForColumns columns = mconcat $ V.toList params
  where
    params = asParam <$> V.zip (V.fromList [0..]) (fmap snd columns)
    asParam (idx, dataType) = contramap (! idx) (vector $ valueEncoderForType dataType)
    vector value =
      HE.param (HE.array (HE.dimension V.foldl (HE.element value)))


data InsertContext = InsertContext
  { insertStatement :: Statement (Vector (Vector A.Value)) GHC.Int.Int64
  , columns :: Vector (Text, Text) }


createInsertContext :: Text -> Pool -> IO InsertContext
createInsertContext table = runReaderT f
  where
    f = do
      currentSchema <- getCurrentSchema
      let relationName = parseTableName currentSchema table
      columns <- getColumns relationName
      let insertStmt = insert relationName columns
      pure $ InsertContext insertStmt columns
