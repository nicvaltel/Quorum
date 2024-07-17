{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


module Lib (runRoutine) where

import Reexport
import Domain.Auth
import qualified Adapter.InMemory.Auth as Mem
import qualified Adapter.PostgreSQL.Auth as PG
import qualified Adapter.Redis.Auth as RDS
import qualified Configuration.Dotenv as Dotenv
import qualified Adapter.RabbitMQ.Common as MQ
import qualified Adapter.RabbitMQ.Auth as MQAuth
import qualified Data.Text as T

import Katip (KatipContextT)
import Logging (withKatip)
import qualified Data.ByteString.Char8 as BSC8
import Control.Exception.Safe (MonadThrow)
import qualified Domain.Posts as P



type LibState = (PG.State, RDS.State, MQ.State, Mem.MemState)

newtype App a = App { unApp :: ReaderT LibState (KatipContextT IO) a  } 
  deriving (Functor, Applicative, Monad, MonadReader LibState, MonadIO, MonadFail, MonadThrow, MonadCatch, KatipContext, Katip)

instance AuthRepo App where
  addAuth = PG.addAuth
  setEmailAsVerified = PG.setEmailAsVerified
  findUserByAuth = PG.findUserByAuth
  findEmailFromUserId = PG.findEmailFromUserId
  

instance EmailVerificationNotif App where
  notifyEmailVerification = MQAuth.notifyEmailVerification

instance SessionRepo App where
  newSession = RDS.newSession
  findUserIdBySessionId = RDS.findUserIdBySessionId

instance  Articles App where
  postActicleUserId = PG.postActicleUserId
  postCommentUserId = PG.postCommentUserId

-- instance SessionRepo (ReaderT LibState (KatipContextT IO)) where
--   newSession = RDS.newSession
--   findUserIdBySessionId = RDS.findUserIdBySessionId
  

withLibState :: (LogEnv -> LibState -> IO ()) -> IO ()
withLibState action = do
  pgCfg <- either error id <$> readDBConfig "db/database.env"
  redisCfg <- either error id <$> readRedisConfig "redis/database.env"

  withKatip $ \le -> do 
    memState <- newTVarIO Mem.initialState
    PG.withState pgCfg $ \pgState ->
      RDS.withState redisCfg $ \redisState ->
        MQ.withState mqCfg 16 $ \mqState -> do
          let libState = (pgState, redisState, mqState, memState)
          action le libState

  where
    mqCfg = "amqp://guest:guest@localhost:5672/%2F"

    readRedisConfig :: String -> IO (Either String String)
    readRedisConfig file = do
      env <- Dotenv.parseFile file
      let result :: Either String String = do
            dbHost <- maybeToRight "No Hostname defined" (lookup "REDIS_HOST" env)
            dbPort :: Int <- maybeToRight "No port number defined" (read <$> lookup "REDIS_PORT" env)
            dbPassword <- maybeToRight "No password defined" (lookup "REDIS_PASSWORD" env)
            dbSelectDb <- maybeToRight "No select db defined" (lookup "REDIS_SELECT_DB" env)
            let configUrl :: String  = printf "redis://%s:%d/%s" dbHost dbPort dbSelectDb 
            pure configUrl
      pure result

    readDBConfig :: String -> IO (Either String PG.Config)
    readDBConfig file = do
      env <- Dotenv.parseFile file
      let result :: Either String PG.Config = do
            dbHost <- maybeToRight "No Hostname defined" (lookup "POSTGRES_HOST" env)
            dbPort :: Int <- maybeToRight "No port number defined" (read <$> lookup "POSTGRES_PORT" env)
            dbName <- maybeToRight "No database name defined" (lookup "POSTGRES_DB" env)
            dbUser <- maybeToRight "No username defined" (lookup "POSTGRES_USER" env)
            dbPassword <- maybeToRight "No password defined" (lookup "POSTGRES_PASSWORD" env)
            configStripeCount <- maybeToRight "No stripe count defined" (read <$> lookup "POSTGRES_STRIPE_COUNT" env)
            dbMaxOpenConnPerStripe <- maybeToRight "No max open connections per stripe defined" (read <$> lookup "POSTGRES_MAX_OPEN_CONN_PER_STRIPE" env)
            dbIdleConnTimeout <- maybeToRight "No stripe count defined" (read <$> lookup "POSTGRES_IDLE_CONN_TIMEOUT" env)
            let configUrl :: String  = printf "postgresql://%s:%s@%s:%d/%s" dbUser dbPassword dbHost dbPort dbName 
            pure PG.Config {PG.configUrl = BSC8.pack configUrl, PG.configStripeCount = configStripeCount, PG.configMaxOpenConnPerStripe = dbMaxOpenConnPerStripe, PG.congigIdleConnTimeout = dbIdleConnTimeout}
      pure result


runState :: LogEnv -> LibState -> App a -> IO a
runState le state =
  runKatipContextT le () mempty 
  . flip runReaderT state 
  . unApp
  
runState' :: LogEnv -> b -> ReaderT b (KatipContextT m) a -> m a
runState' le state =
  runKatipContextT le () mempty 
  . flip runReaderT state 

runRoutine :: IO ()
runRoutine = do
  withLibState $ \le state@(_,_,mqState,_) -> do
    let runner = runState' le state
    MQAuth.init mqState runner
    runner (unApp routine)


routine :: App ()
routine = do
  emailFileContent <- liftIO $ T.pack <$> readFile "test-email.cfg"
  liftIO $ putStrLn emailFileContent
  let email = either undefined id $ mkEmail emailFileContent
  let passw = either undefined id $ mkPassword "123456Hello"
  let auth = Auth email passw
  _ <- register auth
  vCode <- App $ pollNotif email
  -- Just vCode <- App $ Mem.getNotificationsForEmail email
  _ <- verifyEmail vCode
  Right session <- login auth
  Just uId <- resolveSessionId session
  Just registeredEmail <- getUser uId
  Right aId <- postActicle session P.Article{P.articleHead = "Head4", P.articleBody = "Body4..."}
  cId <- postComment session aId P.Comment{P.commentMessage = "Comment Message4..."}
  cId1 <- postComment session 666 P.Comment{P.commentMessage = "Comment Message4..."}
  liftIO $ print (session, uId, registeredEmail, aId, cId, cId1)
  where
    pollNotif email = do
      result <- Mem.getNotificationsForEmail email
      case result of
        Nothing -> pollNotif email
        Just vCode -> return vCode
