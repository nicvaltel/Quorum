{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LibInMemory
     where

import Adapter.InMemory.Auth qualified as M
import Domain.Auth
    ( mkEmail,
      mkPassword,
      getUser,
      resolveSessionId,
      login,
      verifyEmail,
      register,
      SessionRepo(..),
      EmailVerificationNotif(..),
      AuthRepo(..),
      Auth(Auth) )
import Control.Concurrent.STM ( TVar, newTVarIO )
import Control.Monad.Reader
    ( ReaderT(..), MonadIO(..), MonadReader )
import Katip
import Control.Exception (bracket)
import System.IO (stdout)


type State = TVar M.AppState
newtype App a = App {unApp :: ReaderT State (KatipContextT IO) a}
    deriving (Functor, Applicative, Monad, MonadReader State, MonadIO, MonadFail, KatipContext, Katip)

run :: LogEnv -> State -> App a -> IO a
run le state app = 
    runKatipContextT le () mempty $
    runReaderT (unApp app) state


instance AuthRepo App where
    addAuth = M.addAuth 
    setEmailAsVerified = M.setEmailAsVerified
    findUserByAuth  = M.findUserByAuth 
    findEmailFromUserId = M.findEmailFromUserId

instance EmailVerificationNotif App where
    notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
    newSession = M.newSession 
    findUserIdBySessionId = M.findUserIdBySessionId 

withKatip :: (LogEnv ->IO a) -> IO a
withKatip app = do
    bracket createLogEnv closeScribes app
    where
        createLogEnv = do
            logEnv <- initLogEnv "Quorum" "dev"
            stdoutScribe <- mkHandleScribe ColorIfTerminal  stdout (permitItem InfoS) V2
            registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

            

testLib :: IO ()
testLib = do
    state <- newTVarIO M.initialState
    withKatip $ \le -> run le state testAction1
    pure ()

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