{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib
     where



import Adapter.InMemory.Auth qualified as M
import Domain.Auth
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Applicative (Applicative)
import Domain.Auth (AuthRepo)
import Control.Monad.State.Lazy (MonadFail)

type State = TVar M.State
newtype App a = App {unApp :: ReaderT State IO a}
    deriving (Functor, Applicative, Monad, MonadReader State, MonadIO, MonadFail)

run :: State -> App a -> IO a
run state app = runReaderT (unApp app) state


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


testLib :: IO ()
testLib = do
    state <- newTVarIO M.initialState
    run state testAction1
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
