{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
module Adapter.RabbitMQ.Auth where

import Reexport
import Adapter.RabbitMQ.Common
import qualified Adapter.InMemory.Auth as M
import Network.AMQP
import Data.Aeson.TH
import qualified Domain.Auth as D
import Data.Maybe (fromMaybe)
import Safe (lastMay)
import Data.Char (toLower)
import Data.Sequences (splitElem)


data EmailVerificationPayload = EmailVerificationPayload
  { emailVerificationPayloadEmail :: Text
  , emailVerificationPayloadVerificationCode :: Text
  } deriving (Generic, FromJSON, ToJSON)

-- $(let structName = fromMaybe "" . lastMay . splitElem '.' . show $ ''EmailVerificationPayload
--       lowercaseFirst (x:xs) = toLower x : xs
--       lowercaseFirst xs = xs
--       options = defaultOptions {fieldLabelModifier = lowercaseFirst . drop (length structName)}
--   in deriveJSON options ''EmailVerificationPayload)


notifyEmailVerification :: D.Email -> D.VerificationCode -> Rabbit r m ()
notifyEmailVerification email vCode =
  let payload = EmailVerificationPayload
                { emailVerificationPayloadEmail = D.rawEmail email
                , emailVerificationPayloadVerificationCode = vCode
                }
  in publish "auth" "userRegistered" payload

-- consumerEmailVerification :: (MonadIO m, KatipContext m, MonadCatch m) => (m Bool -> IO Bool) -> Message -> IO Bool
consumerEmailVerification :: (KatipContext m, MonadCatch m, Has M.MemState r) => (M.InMemory r m Bool -> b) -> Message -> b
consumerEmailVerification runner msg = 
  runner $ consumeAndProcess msg handler
    where
      -- handler :: (MonadIO m, KatipContext m, MonadCatch m) => EmailVerificationPayload -> M.InMemory r m Bool
      handler payload = do
        case D.mkEmail (emailVerificationPayloadEmail payload) of
          Left err -> withMsgAndErr msg err $ do
            $(logTM) ErrorS "Email format is invalid. Rejecting."
            pure False
          Right email -> do
            let vCode = emailVerificationPayloadVerificationCode payload
            M.notifyEmailVerification email vCode 
            pure True 


init ::  (KatipContext m, MonadCatch m, Has M.MemState r) => State -> (M.InMemory r m Bool -> IO Bool) -> IO ()
init state runner = do
  initQueue state "verifyEmail" "auth" "userRegistered"
  initConsumer state "verifyEmail" (consumerEmailVerification runner)