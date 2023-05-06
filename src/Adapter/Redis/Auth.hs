{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Adapter.Redis.Auth where


import Domain.Auth qualified as D
import Text.StringRandom
import Data.Has
import Database.Redis qualified as R
import UnliftIO (throwString)
import Control.Monad.RWS (MonadReader, MonadIO (liftIO), asks)
import Control.Monad.Catch (MonadThrow)
import Domain.Auth (UserId, SessionId)
import Data.Text.Encoding (encodeUtf8)
import Data.String (fromString)
import Data.ByteString.Char8 (readInt)

type AppState = R.Connection 

type Redis r m = (Has AppState r, MonadReader r m, MonadIO m, MonadThrow m)

-- | Create state from redis url string.
-- format: redis://user:pass@host:port/db
-- sample: redis://abc:def@localhost:6379/0
withAppState :: String -> (AppState -> IO a) -> IO a
withAppState connUrl action = do
  case R.parseConnectInfo connUrl of
    Left _ -> throwString "Invalid Redis conn URL"
    Right connInfo -> do
      conn <- R.checkedConnect connInfo
      action conn

withConn :: Redis r m => R.Redis a -> m a
withConn action = do
  conn <- asks getter
  liftIO $ R.runRedis conn action


newSession :: Redis r m => UserId -> m SessionId
newSession userId = do
  randStr <- liftIO $ stringRandomIO "[A-Za-z0-9]{32}"
  let sessionId = fromString (show userId) <> "_" <> randStr
  reply <- withConn $ R.set (encodeUtf8 sessionId) (fromString $ show userId)
  case reply of
    Right R.Ok -> pure sessionId
    err ->  throwString $ "Unexpected redis error: " <> show err


findUserIdBySessionId :: Redis r m => SessionId -> m (Maybe UserId)
findUserIdBySessionId sessionId = do
  reply <- withConn $ R.get (encodeUtf8 sessionId)
  case reply of
    Right (Just uIdStr) -> pure $ fst <$> readInt uIdStr
    Right Nothing -> pure Nothing
    Left err ->  throwString $ "Unexpected redis error: " <> show err

