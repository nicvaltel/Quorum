{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Adapter.InMemory.Auth where

import Reexport
import qualified Domain.Auth as D
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Domain.Auth (Auth(authEmail))

type MemState = TVar State
type InMemory r m a = (MonadIO m, Has MemState r) => ReaderT r m a

data State = State 
  { stateAuth :: [(D.UserId, D.Auth)]
  , stateUnverifiedEmails :: Map D.VerificationCode D.Email
  , stateVerifiedEmails :: Set D.Email
  , stateUserIdCounter :: Int
  , stateNotifications :: Map D.Email D.VerificationCode
  , stateSessions :: Map D.SessionId D.UserId
  } deriving (Show, Eq)


initialState :: State
initialState = State
  { stateAuth = []
  , stateUnverifiedEmails = Map.empty
  , stateVerifiedEmails = Set.empty
  , stateUserIdCounter = 0
  , stateNotifications = Map.empty
  , stateSessions = Map.empty
  }


findUserIdBySessionId :: D.SessionId -> InMemory r m (Maybe D.UserId)
findUserIdBySessionId sId = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar 
  pure $ Map.lookup sId (stateSessions state)
 

newSession :: D.UserId -> InMemory r m D.SessionId
newSession uId = do
  tvar <- asks getter 
  sId <- liftIO $ (tshow uId <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
  liftIO $ atomically $ do
    state <- readTVar tvar
    let sessions = stateSessions state
        newSessions = Map.insert sId uId sessions
        newState = state { stateSessions = newSessions }
    writeTVar tvar newState
    pure sId

notifyEmailVerification :: D.Email -> D.VerificationCode -> InMemory r m ()
notifyEmailVerification email vCode = do
  tvar <- asks getter
  liftIO $ atomically $ do
    state <- readTVar tvar
    let notifications = stateNotifications state
        newNotifications = Map.insert email vCode notifications
        newState = state {stateNotifications = newNotifications}
    writeTVar tvar newState

getNotificationsForEmail :: D.Email -> InMemory r m (Maybe D.VerificationCode)
getNotificationsForEmail email = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  pure $ Map.lookup email (stateNotifications state)


findEmailFromUserId :: D.UserId -> InMemory r m (Maybe D.Email)
findEmailFromUserId uId = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  pure $ D.authEmail <$> lookup uId (stateAuth state)


findUserByAuth :: D.Auth -> InMemory r m (Maybe (D.UserId, Bool)) -- Bool = email is verified
findUserByAuth auth = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let mayUserId = fst <$> find ((auth ==) . snd) (stateAuth state)
  case mayUserId of
    Nothing -> pure Nothing
    Just uId -> do
      let isVerified = D.authEmail auth `Set.member` stateVerifiedEmails state 
      pure $ Just (uId, isVerified)


orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow Nothing e = throwError e
orThrow (Just a) _ = pure a

setEmailAsVerified :: D.VerificationCode -> InMemory r m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = do
  tvar <- asks getter
  liftIO $ atomically $ runExceptT $ do
    state <- lift $ readTVar tvar
    let mayEmail = Map.lookup vCode (stateUnverifiedEmails state)
    email <- mayEmail `orThrow` D.EmailVerificationErrorInvalidCode
    let mayUserId = fst <$> find ((email ==) . D.authEmail . snd) (stateAuth state)
    uId <- mayUserId `orThrow` D.EmailVerificationErrorInvalidCode
    let newState = state 
          { stateUnverifiedEmails = Map.delete vCode (stateUnverifiedEmails state)
          , stateVerifiedEmails = Set.insert email (stateVerifiedEmails state)
          }
    lift $ writeTVar tvar newState
    pure (uId, email)

addAuth :: D.Auth -> InMemory r m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth auth = do
  tvar <- asks getter
  vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}" 
  liftIO $ atomically $ runExceptT $ do
    state <- lift $ readTVar tvar
    let email = D.authEmail auth
    let emailAlreadyTaken = email `elem` [ authEmail | (_, D.Auth{authEmail}) <- stateAuth state]
    when emailAlreadyTaken $ throwError D.RegistrationErrorEmailTaken
    let newUserId = stateUserIdCounter state + 1
    let newState = state
          { stateAuth = (newUserId, auth) : stateAuth state
          , stateUnverifiedEmails = Map.insert vCode email (stateUnverifiedEmails state)
          , stateUserIdCounter = newUserId
          }
    lift $ writeTVar tvar newState
    pure (newUserId, vCode)




runTest :: IO ()
runTest = do
  let email = D.mkEmail "hello@test.com"
  let passw = D.mkPassword "12345ASDFqwer"
  let auth = either undefined id $ D.Auth <$> email <*> passw 
  s <- newTVarIO initialState 
  
  res0 <- flip runReaderT s $ do
      vCode <- addAuth auth
      user <- findUserByAuth auth
      email' <- findEmailFromUserId 1
      sId <- newSession 1
      uId <- findUserIdBySessionId sId
      pure (vCode, user, email', sId, uId)
  print res0
  pure ()