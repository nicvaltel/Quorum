{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Domain.Auth
  ( -- * Types
    Auth (..),
    Email,
    mkEmail,
    rawEmail,
    Password,
    mkPassword,
    rawPassword,
    UserId,
    VerificationCode,
    SessionId,
    RegistrationError (..),
    EmailVerificationError (..),
    LoginError (..),

    -- * Ports
    AuthRepo (..),
    EmailVerificationNotif (..),
    SessionRepo (..),

    -- * Use cases
    register,
    verifyEmail,
    login,
    resolveSessionId,
    getUser,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), MonadTrans (lift), runExceptT)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Domain.User

type SessionId = Text

data Auth = Auth
  { authEmail :: Email,
    authPassword :: Password
  }
  deriving (Show, Eq)

data RegistrationError = RegistrationErrorEmailTaken
  deriving (Show, Eq)

data EmailVerificationError = EmailVerificationErrorInvalidCode
  deriving (Show, Eq)

data LoginError
  = LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified
  deriving (Show, Eq)

type VerificationCode = Text

class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError VerificationCode)
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError ())
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUserId :: UserId -> m (Maybe Email)

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)

register :: (AuthRepo m, EmailVerificationNotif m) => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
  vCode <- ExceptT (addAuth auth)
  lift $ notifyEmailVerification (authEmail auth) vCode

verifyEmail :: (AuthRepo m) => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail = setEmailAsVerified

instance AuthRepo IO where
  addAuth Auth {authEmail, authPassword} = do
    T.putStrLn $ "Adding auth: " <> rawEmail authEmail
    pure $ Right "fake verification code"

instance EmailVerificationNotif IO where
  notifyEmailVerification email vcode =
    T.putStrLn $ "Notify " <> rawEmail email <> " - " <> vcode

login :: (AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just (_, False) -> throwError LoginErrorEmailNotVerified
    Just (userId, True) -> lift $ newSession userId

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId
