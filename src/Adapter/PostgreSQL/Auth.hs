{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module Adapter.PostgreSQL.Auth
  ( Config(..)
  , State
  , withState
  , addAuth
  , setEmailAsVerified
  , findUserByAuth
  , findEmailFromUserId
  , postActicleUserId
  ) where

import Reexport
import Database.PostgreSQL.Simple (Connection, withTransaction, close, connectPostgreSQL, query, Only (..), SqlError (..))
import Database.PostgreSQL.Simple.Migration
import Data.Pool
import qualified Domain.Auth as D
import Domain.Auth (Auth(authEmail,authPassword))
import Data.ByteString (isInfixOf)
import qualified Data.Text as T
import Domain.Posts (Article(..), ArticleError(..), ArticleId)
import Control.Exception.Safe (MonadThrow)


type State = Pool Connection

type PG r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)


data Config = Config
  { configUrl :: ByteString
  , configStripeCount :: Int
  , configMaxOpenConnPerStripe :: Int
  , congigIdleConnTimeout :: Double --in seconds NominalDiffTime
  }

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO . withResource pool $ \conn -> action conn

addAuth :: PG r m => D.Auth -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth D.Auth{authEmail, authPassword} = do
  let rawEmail = D.rawEmail authEmail
  let rawPassword = D.rawPassword authPassword
  
  vCode <- liftIO $ do
    r <- stringRandomIO "[A-Za-z0-9]{16}" 
    pure (tshow rawEmail <> "_" <> r)

  result <- withConn $ \conn ->
    try $ query conn qry (rawEmail, rawPassword, vCode)

  case result of
    Right[Only uId] -> pure (Right (uId, vCode))
    Right _ -> throwString "Should not happen: PG doesn't return userId"
    Left err@SqlError{sqlState = state, sqlErrorMsg = msg} ->
      if state == "23505" && "auths_email_ley" `isInfixOf` msg
        then pure $ Left D.RegistrationErrorEmailTaken
        else throwString ( "Unhandled PG exception: " <> show err)
  where
    qry = "insert into auths \
            \(email, pass, email_verification_code, is_email_verified) \
            \values (?, crypt(?, gen_salt('bf')),?,'f') returning id"


setEmailAsVerified :: PG r m => D.VerificationCode -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = do
    result <- withConn $ \conn -> query conn qry (Only vCode)
    case result of
      [(uId, mail)] -> case D.mkEmail mail of
        Right email -> pure $ Right (uId, email)
        _ -> throwString $ "Should not happen: email in DB is not valid: " <> T.unpack mail
      _ -> pure $ Left D.EmailVerificationErrorInvalidCode
  where
      qry = "update auths \
              \set is_email_verified = 't' \
              \where email_verification_code = ? \
              \returning id, cast (email as text)"

findUserByAuth :: PG r m => D.Auth -> m (Maybe (D.UserId, Bool)) -- Bool = email is verified
findUserByAuth D.Auth{authEmail, authPassword} = do
  let rawEmail = D.rawEmail authEmail
  let rawPassword = D.rawPassword authPassword
  result <- withConn $ \conn -> query conn qry (rawEmail, rawPassword)
  case result of
    [(uId, isVerif)] -> pure $ Just (uId, isVerif)
    _ -> pure Nothing
  where
    qry = "select id, is_email_verified from auths where email = ? and pass = crypt(?, pass)"

findEmailFromUserId :: PG r m => D.UserId -> m (Maybe D.Email)
findEmailFromUserId uId = do
  result <- withConn $ \conn -> query conn qry (Only uId)
  case result of
    [Only mail] -> case D.mkEmail mail of
        Right email -> pure $ Just email
        _ -> throwString $ "Should not happen: email in DB is not valid: " <> T.unpack mail
    _ -> pure Nothing


  where
    qry = "select cast(email as text) from auths where id = ?"


postActicleUserId :: PG r m => D.UserId -> Article -> m (Either ArticleError ArticleId) 
postActicleUserId uId Article{articleHead, articleBody} = do
      result <- withConn $ \conn -> query conn qry (uId, articleHead, articleBody)
      case result of
        [Only articleId] -> pure (Right articleId)
        _ -> throwString $ "Should not happen: userId is not in DB: " <> show uId
  where
    qry = "insert into articles (user_id, head, body) values (?, ?, ?) returning id"


withPool :: Config -> (State -> IO a) -> IO a
withPool cfg action = do
  bracket initPool cleanPool action
  where
    initPool = newPool $ defaultPoolConfig 
      openConn 
      closeConn
      (congigIdleConnTimeout cfg)
      (configMaxOpenConnPerStripe cfg * configStripeCount cfg)

    cleanPool = destroyAllResources
    openConn = connectPostgreSQL (configUrl cfg)
    closeConn = close

withState :: Config -> (State -> IO a) -> IO a
withState cfg action =
  withPool cfg $ \state -> do
    migrate state
    action state

migrate :: State -> IO ()
migrate pool = withResource pool $ \conn -> do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> throwString err
    _ -> pure ()
    
    where
      cmds = 
        [ MigrationInitialization
        , MigrationDirectory "src/Adapter/PostgreSQL/Migrations"
        ]


migrate' :: Connection -> IO ()
migrate' conn = do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> throwString err
    _ -> pure ()
    
    where
      cmds = 
        [ MigrationInitialization
        , MigrationDirectory "src/Adapter/PostgreSQL/Migrations"
        ]