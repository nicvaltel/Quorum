{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Adapter.PostgreSQL.Auth
  ( AppState,
    DBConfig,
    readDBConfig,
    withAppState,
    addAuth,
    setEmailAsVerified,
    findUserByAuth,
    findEmailFromUserId,
  )
where

import Configuration.Dotenv (parseFile)
import Control.Exception (bracket, try)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS (MonadReader, asks)
import Data.ByteString (isInfixOf)
import Data.Either.Combinators (maybeToRight)
import Data.Has (Has (getter))
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool, withResource)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    SqlError (sqlErrorMsg, sqlState),
    close,
    connectPostgreSQL,
    execute,
    query,
    withTransaction,
  )
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand (MigrationDirectory, MigrationInitialization),
    MigrationResult (MigrationError),
    runMigrations,
  )
import Domain.Auth (Auth, Email, EmailVerificationError, RegistrationError, UserId, VerificationCode)
import Domain.Auth qualified as D
import Text.Printf (printf)
import Text.StringRandom (stringRandomIO)
import UnliftIO (throwString)

type AppState = Pool Connection

type PG r m = (Has (Pool Connection) r, MonadReader r m, MonadIO m, MonadThrow m)

data DBConfig = DBConfig
  { dbHost :: String,
    dbPort :: Int,
    dbName :: String,
    dbUser :: String,
    dbPassword :: String,
    dbStripeCount :: Int,
    dbMaxOpenConnPerStripe :: Int,
    dbIdleConnTimeout :: Double
  }
  deriving (Show)

readDBConfig :: String -> IO (Either String DBConfig)
readDBConfig file = do
  env <- parseFile file
  let result :: Either String DBConfig = do
        dbHost <- maybeToRight "No Hostname defined" (lookup "POSTGRES_HOST" env)
        dbPort <- maybeToRight "No port number defined" (read <$> lookup "POSTGRES_PORT" env)
        dbName <- maybeToRight "No database name defined" (lookup "POSTGRES_DB" env)
        dbUser <- maybeToRight "No username defined" (lookup "POSTGRES_USER" env)
        dbPassword <- maybeToRight "No password defined" (lookup "POSTGRES_PASSWORD" env)
        dbStripeCount <- maybeToRight "No stripe count defined" (read <$> lookup "POSTGRES_STRIPE_COUNT" env)
        dbMaxOpenConnPerStripe <- maybeToRight "No max open connections per stripe defined" (read <$> lookup "POSTGRES_MAX_OPEN_CONN_PER_STRIPE" env)
        dbIdleConnTimeout <- maybeToRight "No stripe count defined" (read <$> lookup "POSTGRES_IDLE_CONN_TIMEOUT" env)
        pure DBConfig {dbHost, dbPort, dbName, dbUser, dbPassword, dbStripeCount, dbMaxOpenConnPerStripe, dbIdleConnTimeout}
  pure result

openAndClose :: IO ()
openAndClose = do
  eitherDBConf <- readDBConfig "db/database.env"
  case eitherDBConf of
    Left err -> putStrLn err
    Right conf -> do
      print conf
      let connString :: String = printf "host='%s' port=%d dbname='%s' user='%s' password='%s'" conf.dbHost conf.dbPort conf.dbName conf.dbUser conf.dbPassword
      conn <- connectPostgreSQL (fromString connString)
      -- multiUpdates conn
      -- migrate conn
      putStrLn "Migration done"
      close conn

migrate :: Pool Connection -> IO ()
migrate pool = withResource pool $ \conn -> do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> throwString err
    _ -> pure ()
  where
    cmds = [MigrationInitialization, MigrationDirectory "src/Adapter/PostgreSQL/Migrations"]

multiUpdates :: Connection -> IO ()
multiUpdates conn = withTransaction conn $ do
  execute conn "update auths set is_email_verified = ?" (Only True)
  execute conn "update auths set email = ? where id = ?" ("hello@example.org" :: Text, 6 :: Int)
  pure ()

withPool :: DBConfig -> (Pool Connection -> IO a) -> IO a
withPool conf action = do
  bracket initPool cleanPool action
  where
    initPool = newPool $ defaultPoolConfig openConn close conf.dbIdleConnTimeout conf.dbMaxOpenConnPerStripe
    cleanPool = destroyAllResources
    openConn = connectPostgreSQL (fromString connectString)

    connectString :: String
    connectString = printf "host='%s' port=%d dbname='%s' user='%s' password='%s'" conf.dbHost conf.dbPort conf.dbName conf.dbUser conf.dbPassword

withAppState :: DBConfig -> (AppState -> IO a) -> IO a
withAppState conf action =
  withPool conf $ \poolConnection -> do
    migrate poolConnection
    action poolConnection

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO $ withResource pool (\conn -> action conn)

addAuth :: PG r m => Auth -> m (Either RegistrationError (UserId, VerificationCode))
addAuth auth = do
  let rawEmail_ = D.rawEmail auth.authEmail
  let rawPassword_ = D.rawPassword auth.authPassword
  vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}" >>= \r -> pure (rawEmail_ <> "_" <> r)
  result <- withConn $ \conn -> try $ query conn qryStr (rawEmail_, rawPassword_, vCode)
  case result of
    Right [Only uId] -> pure $ Right (uId, vCode)
    Right _ -> throwString "Should not happen: PG does not return userId"
    Left (err :: SqlError) ->
      if sqlState err == "23505" && "auth_email_key" `isInfixOf` sqlErrorMsg err
        then pure $ Left D.RegistrationErrorEmailTaken
        else throwString $ "Unhandeled PG exception: " <> show err
  where
    qryStr = "insert into auths \
          \(email, pass, email_verification_code, is_email_verified) \
          \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"

setEmailAsVerified :: PG r m => VerificationCode -> m (Either EmailVerificationError (UserId, Email))
setEmailAsVerified vCode = do
  mayUserIdEmail <- withConn $ \conn -> query conn qryStr (Only vCode)
  case mayUserIdEmail of
    [] -> pure $ Left D.EmailVerificationErrorInvalidCode
    [(uId :: Int, emailText :: Text)] -> do
      case D.mkEmail emailText of
        Right email -> pure $ Right (uId, email)
        _ -> throwString $ "Should not happen: email in DB is not valid email = " ++ Text.unpack emailText
    _ -> throwString $ "Should not happen: multiple verification codes in auths table with vCode = " ++ Text.unpack vCode
  where
    qryStr = "update auths set is_email_verified = 't' where email_verification_code = ? returning id, cast(email as text)"

findUserByAuth :: PG r m => Auth -> m (Maybe (UserId, Bool))
findUserByAuth auth = do
  let rawEmail = D.rawEmail auth.authEmail
  let rawPassword = D.rawPassword auth.authPassword
  result :: [(Int, Bool)] <- withConn $ \conn -> query conn qryStr (rawEmail, rawPassword)
  case result of
    [] -> pure Nothing
    [(uId, isVerified)] -> pure $ Just (uId, isVerified)
    _ -> throwString $ "Should not happen: several emails in auth table in DB - email = " ++ Text.unpack (D.rawEmail auth.authEmail)
  where
    qryStr = "select id, is_email_verified from auths where email = ? and pass = crypt(?,pass)"

findEmailFromUserId :: PG r m => UserId -> m (Maybe Email)
findEmailFromUserId userId = do
  result :: [Only Text] <- withConn $ \conn -> query conn qryStr (Only userId)
  case result of
    [] -> pure Nothing
    [Only emailText] -> case D.mkEmail emailText of
      Right email -> pure $ Just email
      _ -> throwString $ "Should not happen: email in DB is not valid email = " ++ Text.unpack emailText
    _ -> throwString $ "Should not happen: several userId's in auth table in DB - id = " ++ show userId
  where
    qryStr = "select cast(email as text) from auths where id = ?"
