

module Cli where

import           Data.Text           (Text)
import           Options.Applicative (auto, help, long, option, optional,
                                      showDefault, str, value, (<**>))
import qualified Options.Applicative as P


data Args = Args
  { dbUri :: [Text]
  , concurrency :: Int
  , bulkSize :: Int
  , avgRate :: Maybe Double
  , table :: Text }
  deriving (Show)


argsParser :: P.Parser Args
argsParser = Args
  <$> P.some (option str
      ( long "db-uri" <> help "database uri" ))
  <*> option auto
    ( long "concurrency" 
    <> showDefault
    <> value 15 )
  <*> option auto
    ( long "bulk-size"
    <> showDefault
    <> value 1000 )
  <*> (optional 
    $ option auto 
    $ long "avg-rate")
  <*> option str
    ( long "table" )


getArgs :: IO Args
getArgs = P.execParser opts
  where
    opts = P.info (argsParser <**> P.helper)
      ( P.fullDesc <> P.progDesc "Ingest data into CrateDB" )
