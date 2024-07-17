{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Adapter.Redis.Auth
  ( State
  , withState
  , newSession
  , findUserIdBySessionId
  ) where


import Reexport
import qualified Domain.Auth as D
import qualified Database.Redis as R
import Data.Text.Encoding (encodeUtf8)
import ClassyPrelude (fromString, Utf8 (decodeUtf8), readMay)
import qualified Data.Text as T
import Control.Exception.Safe (MonadThrow)


type State = R.Connection
type Redis r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

withState :: String -> (State -> IO a) -> IO a
withState connUrl action = do
  case R.parseConnectInfo connUrl of
    Left _ -> throwString "Invalid Redis conn URL"
    Right connInfo -> do
      conn <- R.checkedConnect connInfo
      action conn

withConn :: Redis r m => R.Redis a -> m a
withConn action = do
  conn <- asks getter
  liftIO $ R.runRedis conn action

newSession :: Redis r m => D.UserId -> m D.SessionId
newSession userId = do
  sId <- liftIO $ stringRandomIO "[A-Za-z0-9]{32}"
  result <- withConn $ R.set (encodeUtf8 sId) (fromString . show $ userId)
  case result of
    Right R.Ok -> pure sId
    err -> throwString $ "Unexpected redis error: " <> show err

findUserIdBySessionId :: Redis r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sId = do
  result <- withConn $ R.get (encodeUtf8 sId)
  case result of
    Right (Just uIdStr) -> pure . readMay . T.unpack . decodeUtf8 $ uIdStr
    err -> throwString $ "Unexpected redis error: " <> show err



runExample :: IO ()
runExample = do
  conn <- R.checkedConnect R.defaultConnectInfo
  world <- R.runRedis conn $ do
    R.set "hello" "world"
    R.get "hello"
  print world
  
  let cfg = R.defaultConnectInfo
        { R.connectHost = "127.0.0.1"
        , R.connectMaxConnections = 100
        }
  
  pure ()