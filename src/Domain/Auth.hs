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
    ( Password,
      Email,
      UserId,
      rawEmail,
      mkEmail,
      rawPassword,
      mkPassword )
import Katip
import Katip.Monadic (KatipContext)

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
  addAuth :: Auth -> m (Either RegistrationError (UserId, VerificationCode))
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError (UserId, Email))
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUserId :: UserId -> m (Maybe Email)

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)

register :: (KatipContext m, AuthRepo m, EmailVerificationNotif m) => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
  (userId, vCode) <- ExceptT (addAuth auth)
  lift $ notifyEmailVerification auth.authEmail vCode
  withUserIdContext userId $
    $(logTM) InfoS $ ls (rawEmail auth.authEmail) <> " is registered successfully"

verifyEmail :: (KatipContext m, AuthRepo m) => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail vCode = runExceptT $ do
  (userId, email) <- ExceptT $ setEmailAsVerified vCode
  withUserIdContext userId $
    $(logTM) InfoS $ ls (rawEmail email) <> " is verified successully"

instance AuthRepo IO where
  addAuth Auth {authEmail, authPassword} = do
    T.putStrLn $ "Adding auth: " <> rawEmail authEmail
    pure $ Right (0, "fake verification code")

instance EmailVerificationNotif IO where
  notifyEmailVerification email vcode =
    T.putStrLn $ "Notify " <> rawEmail email <> " - " <> vcode

login :: (KatipContext m, AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just (_, False) -> throwError LoginErrorEmailNotVerified
    Just (userId, True) -> withUserIdContext userId . lift $ do
      sId <- newSession userId
      $(logTM) InfoS $ ls (rawEmail auth.authEmail) <> " logged in successfully"
      pure sId

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId


withUserIdContext :: (KatipContext m) => UserId -> m a -> m a
withUserIdContext userId = katipAddContext (sl "userId" userId)

