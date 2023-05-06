{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

module LibPostrgeSQL where

import Adapter.InMemory.Auth qualified as M
import Adapter.PostgreSQL.Auth qualified as PG
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Exception (bracket)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader
  ( MonadIO (..),
    MonadReader,
    ReaderT (..),
  )
import Data.Text (Text)
import Domain.Auth
  ( Auth (Auth),
    AuthRepo (..),
    EmailVerificationNotif (..),
    SessionRepo (..),
    getUser,
    login,
    mkEmail,
    mkPassword,
    register,
    resolveSessionId,
    verifyEmail,
  )
import GHC.IO.Handle.Internals (mkHandle)
import Katip
import Katip.Monadic (KatipContext)
import System.IO (stdout)
import qualified Control.Monad.State.Lazy as PG
import qualified Adapter.PostgreSQL.Auth as PG

type State = (PG.AppState, TVar M.State)

newtype App a = App {unApp :: ReaderT State (KatipContextT IO) a}
  deriving (Functor, Applicative, Monad, MonadReader State, MonadIO, MonadFail, KatipContext, Katip, MonadThrow)

run :: LogEnv -> State -> App a -> IO a
run le state app =
  runKatipContextT le () mempty $
    runReaderT (unApp app) state

instance AuthRepo App where
  addAuth = PG.addAuth
  setEmailAsVerified = PG.setEmailAsVerified
  findUserByAuth = PG.findUserByAuth
  findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
  newSession = M.newSession
  findUserIdBySessionId = M.findUserIdBySessionId

withKatip :: (LogEnv -> IO a) -> IO a
withKatip app = do
  bracket createLogEnv closeScribes app
  where
    createLogEnv = do
      logEnv <- initLogEnv "Quorum" "dev"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

testLib :: IO ()
testLib = withKatip $ \le -> do
    Right pgCfg <- PG.readDBConfig "db/database.env"
    mState <- newTVarIO M.initialState
    PG.withAppState pgCfg $ \pgState -> run le (pgState, mState) testAction1


testAction1 :: App ()
testAction1 = do
  let Right email = mkEmail "theemail@mail.org"
  let Right password = mkPassword "77788XCVqwe"
  let auth = Auth email password
  register auth
  Just vCode <- M.getNotificationsForEmail email
  verifyEmail vCode
  Right sessionId <- login auth
  Just uId <- resolveSessionId sessionId
  Just registeredEmail <- getUser uId
  liftIO $ print (sessionId, uId, registeredEmail)
