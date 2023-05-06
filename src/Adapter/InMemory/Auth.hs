{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Adapter.InMemory.Auth
  ( 
    AppState(..),
    initialState,
    addAuth,
    setEmailAsVerified,
    findUserByAuth,
    findEmailFromUserId,
    notifyEmailVerification,
    newSession,
    findUserIdBySessionId,
    getNotificationsForEmail
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError), MonadTrans (lift), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS (MonadReader, asks)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Has (Has (getter))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Tuple (swap)
import Domain.Auth (Auth (Auth, authEmail), Email, EmailVerificationError, RegistrationError, SessionId, UserId, VerificationCode)
import Domain.Auth qualified as D
import Text.StringRandom (stringRandomIO)

data AppState = AppState
  { stateAuths :: [(UserId, Auth)],
    stateUnverifiedEmails :: Map VerificationCode (UserId, Email),
    stateVerifiedEmails :: Set Email,
    stateUserIdCounter :: Int,
    stateNotifications :: Map Email VerificationCode,
    stateSessions :: Map SessionId UserId
  }
  deriving (Show, Eq)

type InMemory r m = (Has (TVar AppState) r, MonadReader r m, MonadIO m)

initialState :: AppState
initialState =
  AppState
    { stateAuths = [],
      stateUnverifiedEmails = Map.empty,
      stateVerifiedEmails = Set.empty,
      stateUserIdCounter = 0,
      stateNotifications = Map.empty,
      stateSessions = Map.empty
    }

addAuth :: InMemory r m => Auth -> m (Either RegistrationError (UserId, VerificationCode))
addAuth auth = do
  tvar :: TVar AppState <- asks getter
  vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
  liftIO . atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let email = auth.authEmail
    let isDuplicate = any (\(_, Auth {authEmail}) -> email == authEmail) state.stateAuths
    when isDuplicate $ throwError D.RegistrationErrorEmailTaken
    let newUserId = state.stateUserIdCounter + 1
    let newAuths = (newUserId, auth) : state.stateAuths
    let newUnverifieds = Map.insert vCode (newUserId, auth.authEmail) state.stateUnverifiedEmails
    let newState = state{stateAuths = newAuths, stateUserIdCounter = newUserId, stateUnverifiedEmails = newUnverifieds}
    lift $ writeTVar tvar newState
    pure (newUserId, vCode)

setEmailAsVerified :: InMemory r m => VerificationCode -> m (Either EmailVerificationError (UserId, Email))
setEmailAsVerified vCode = do
  tvar :: TVar AppState <- asks getter
  liftIO . atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let mayUsedIdEmail = Map.lookup vCode state.stateUnverifiedEmails
    case mayUsedIdEmail of
      Nothing -> throwError D.EmailVerificationErrorInvalidCode
      Just (uId, email) -> do
        let newUnverifiedEmails = Map.delete vCode state.stateUnverifiedEmails
        let newVerifiedEmails = Set.insert email state.stateVerifiedEmails
        lift $ writeTVar tvar state{stateUnverifiedEmails = newUnverifiedEmails, stateVerifiedEmails = newVerifiedEmails}
        pure (uId, email)

findUserByAuth :: InMemory r m => Auth -> m (Maybe (UserId, Bool))
findUserByAuth auth = do
  tvar :: TVar AppState <- asks getter
  state <- liftIO $ readTVarIO tvar
  let mayUserId = lookup auth (map swap state.stateAuths)
  case mayUserId of
    Nothing -> pure Nothing
    Just userId -> pure $ Just (userId, auth.authEmail `Set.member` state.stateVerifiedEmails)

findEmailFromUserId :: InMemory r m => UserId -> m (Maybe Email)
findEmailFromUserId userId = do
  tvar :: TVar AppState <- asks getter
  state <- liftIO $ readTVarIO tvar
  pure $ D.authEmail <$> lookup userId state.stateAuths

notifyEmailVerification :: InMemory r m => Email -> VerificationCode -> m ()
notifyEmailVerification email vCode = do
  tvar :: TVar AppState <- asks getter
  liftIO $
    atomically $ do
      state <- readTVar tvar
      let newNotifications = Map.insert email vCode state.stateNotifications
      writeTVar tvar state {stateNotifications = newNotifications}

newSession :: InMemory r m => UserId -> m SessionId
newSession userId = do
  tvar :: TVar AppState <- asks getter
  randStr <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
  let sessionId = Text.pack (show userId) <> randStr
  liftIO $
    atomically $ do
      state <- readTVar tvar
      let newSessions = Map.insert sessionId userId state.stateSessions
      writeTVar tvar (state {stateSessions = newSessions})
  pure sessionId

findUserIdBySessionId :: InMemory r m => SessionId -> m (Maybe UserId)
findUserIdBySessionId sessionId = do
  tvar :: TVar AppState <- asks getter
  state <- liftIO $ readTVarIO tvar
  pure $ Map.lookup sessionId state.stateSessions

getNotificationsForEmail :: InMemory r m => Email -> m (Maybe VerificationCode)
getNotificationsForEmail email = do
  tvar :: TVar AppState <- asks getter
  state <- liftIO $ readTVarIO tvar
  pure $ Map.lookup email state.stateNotifications

test :: IO ()
test = do
  let email = D.mkEmail "gogogo@test.com"
  let passw = D.mkPassword "77788XCVqwe"
  let Right auth = do
        eml <- email
        pwd <- passw
        pure $ D.Auth eml pwd
  s <- newTVarIO initialState
  -- runReaderT (addAuth auth) s >>= print
  -- runReaderT (findUserByAuth auth) s >>= print
  -- runReaderT (findEmailFromUserId 1) s >>= print
  runTest addAuth s auth
  runTest findUserByAuth s auth
  runTest findEmailFromUserId s 1
  sessionId <- runTest newSession s 1
  runTest findUserIdBySessionId s sessionId
  pure ()
  where
    runTest :: Show b => (a -> ReaderT (TVar AppState) IO b) -> TVar AppState -> a -> IO b
    runTest f s a = runReaderT (f a) s >>= \b -> print b >> pure b
