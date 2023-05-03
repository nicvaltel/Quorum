{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Adapter.PostgreSQL.Auth where

import Data.Either.Combinators (maybeToRight)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import UnliftIO (throwString)
import Configuration.Dotenv (parseFile)
import Text.Printf (printf)
import Data.String (fromString)

migrate :: Connection -> IO ()
migrate conn = do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> throwString err
    _ -> pure ()
  where
    cmds = [MigrationInitialization, MigrationDirectory "src/Adapter/PostgreSQL/Migrations"]

data DBConfig = DBConfig
  { dbHost :: String,
    dbPort :: String,
    dbName :: String,
    dbUser :: String,
    dbPassword :: String
  }
  deriving (Show)



readDBConfig :: String -> IO (Either String DBConfig)
readDBConfig file = do
  env <- parseFile file
  let result :: Either String DBConfig = do
        dbHost <- maybeToRight "No Hostname defined" (lookup "POSTGRES_HOST" env)
        dbPort <- maybeToRight "No port number defined" (lookup "POSTGRES_PORT" env)
        dbName <- maybeToRight "No database name defined" (lookup "POSTGRES_DB" env)
        dbUser <- maybeToRight "No username defined" (lookup "POSTGRES_USER" env)
        dbPassword <- maybeToRight "No password defined" (lookup "POSTGRES_PASSWORD" env)
        pure DBConfig {dbHost, dbPort, dbName, dbUser, dbPassword}
  pure result



openAndClose :: IO ()
openAndClose = do
  eitherDBConf <- readDBConfig "db/database.env"
  case eitherDBConf of
    Left err -> putStrLn err
    Right conf -> do
      let connString :: String = printf "host='%s' port=%s dbname='%s' user='%s' password='%s'" conf.dbHost conf.dbPort conf.dbName conf.dbUser conf.dbPassword
      conn <- connectPostgreSQL (fromString connString)
      multiUpdates conn
      -- migrate conn
      putStrLn "Migration done"
      close conn


multiUpdates :: Connection -> IO ()
multiUpdates conn = withTransaction conn $ do
  execute conn "update auths set is_email_verified = ?" (Only True)
  execute conn "update auths set email = ? where id = ?" ("hello@example.org" :: Text, 6 :: Int)
  pure ()