{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Domain.Auth
  ( Auth(..)
  , Email
  , Password
  , RegistrationError(..)
  , AuthRepo(..)
  , EmailVerificationNotif(..)
  , SessionRepo(..)
  , Articles(..)
  , EmailVerificationError(..)
  , VerificationCode
  , SessionId
  , UserId
  , rawEmail
  , rawPassword
  , getUser
  , resolveSessionId
  , login
  , verifyEmail
  , register
  , mkEmail
  , mkPassword
  , postActicle
  , postComment
  ) where

import Reexport
import Domain.Validation
import Text.Regex.PCRE.Heavy
import Domain.Posts (PostError (..), Article (..), ArticleId, Comment, CommentId)

type VerificationCode = Text

type UserId = Int

type SessionId = Text

data Auth = Auth
  { authEmail :: Email
  , authPassword :: Password
  } deriving (Show, Eq)


data RegistrationError = RegistrationErrorEmailTaken
  deriving (Show, Eq)

data EmailVerificationError = EmailVerificationErrorInvalidCode
  deriving (Show, Eq)

data LoginError =  LoginErrorInvalidAuth | LoginErrorEmailNotVerified
  deriving (Show, Eq)

newtype Email = Email {emailRaw :: Text} deriving (Show, Eq, Ord)

rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [Text] Email
mkEmail =
  validate Email
    [ regexMatches
        [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
        "Not a valid email"
    ]

newtype Password = Password { passwordRaw :: Text} deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
    [ lengthBetween 5 50 "Should between 5 and 50"
    , regexMatches [re|\d|] "Should contain number"
    , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
    , regexMatches [re|[a-z]|] "Should contain lowercase letter"
    ]



class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError (UserId, VerificationCode))
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError (UserId, Email))
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool)) -- Bool = email is verified
  findEmailFromUserId :: UserId -> m (Maybe Email)

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

class Monad m => Articles m where
  postActicleUserId :: UserId -> Article -> m (Either PostError ArticleId) 
  postCommentUserId :: UserId -> ArticleId -> Comment -> m (Either PostError CommentId)

withUserIdContext :: (KatipContext m) => UserId -> m a -> m a
withUserIdContext uId = katipAddContext (sl "userId" uId)

register :: (KatipContext m, AuthRepo m, EmailVerificationNotif m) => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
  (uId, vCode) <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email vCode
  withUserIdContext uId $
    $(logTM) InfoS $ ls (rawEmail email) <> " is registered successfully"
  
verifyEmail :: (KatipContext m, AuthRepo m) => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail vCode = runExceptT $ do
  (uId, email) <- ExceptT $ setEmailAsVerified vCode
  withUserIdContext uId $
    $(logTM) InfoS $ ls (rawEmail email) <> " is verified successfully"
  pure ()

login :: (KatipContext m, AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just (_, False) -> throwError LoginErrorEmailNotVerified
    Just (uId, _) -> withUserIdContext uId . lift $ do 
      sId <- newSession uId
      $(logTM) InfoS $ ls (rawEmail $ authEmail auth) <> " logged in successfully"
      pure sId


postActicle :: (KatipContext m, SessionRepo m, Articles m)  => SessionId -> Article -> m (Either PostError ArticleId) 
postActicle sId article = do
  mayUserId <- findUserIdBySessionId sId
  case mayUserId of
    Nothing -> pure $ Left PostErrorSessionInactive
    Just uId -> withUserIdContext uId $ do
      $(logTM) InfoS $ ls (show article) <> " post successfully" 
      postActicleUserId uId article 

postComment :: (KatipContext m, SessionRepo m, Articles m)  => SessionId -> ArticleId -> Comment -> m (Either PostError CommentId) 
postComment sId aId comment = do
  mayUserId <- findUserIdBySessionId sId
  case mayUserId of
    Nothing -> pure $ Left PostErrorSessionInactive
    Just uId -> do
      result <- postCommentUserId uId aId comment
      case result of
        Left err -> pure (Left err)
        Right cId -> withUserIdContext uId $ do
          $(logTM) InfoS $ ls (show comment) <> " post successfully" 
          pure (Right cId)

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId
