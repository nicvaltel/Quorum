{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Adapter.RabbitMQ.Common where

import Reexport
import Network.AMQP


data State = State
  { statePublisherChan :: Channel
  , stateConsumerChan :: Channel
}

type Rabbit r m a = (MonadIO m, Has State r) => ReaderT r m a

withState :: String -> Integer -> (State -> IO a) -> IO a
withState connUri prefetchCount action = bracket initState destroyState action'
  where
    initState = do
      publisher <- openConnAndChan
      consumer <- openConnAndChan
      pure (publisher, consumer)

    openConnAndChan = do
      conn <- openConnection'' . fromURI $ connUri
      chan <- openChannel conn
      qos chan 0 (fromInteger prefetchCount) True
      pure (conn, chan)
    
    destroyState ((conn1,_), (conn2,_)) = do
      closeConnection conn1
      closeConnection conn2

    action' ((_, pubChan), (_, conChan)) = action (State pubChan conChan)


initExchange :: State -> Text -> IO ()
initExchange (State pubChan _) exchangeName = do
  let exchange = newExchange 
        { exchangeName
        , exchangeType = "topic"
        }
  declareExchange pubChan exchange


initQueue :: State -> Text -> Text -> Text -> IO ()
initQueue state@(State pubChan _) queueName exchangeName routingKey = do
  initExchange state exchangeName
  void $ declareQueue pubChan (newQueue {queueName})
  bindQueue pubChan queueName exchangeName routingKey

initConsumer :: State -> Text -> (Message -> IO Bool) -> IO ()
initConsumer (State _ conChan) queueName handler = do
  void . consumeMsgs conChan queueName Ack $ \(msg, env) -> void . fork $ do
    result <- handler msg
    if result 
      then ackEnv env 
      else rejectEnv env False


publish :: ToJSON a => Text -> Text -> a -> Rabbit r m ()
publish exchange routingKey payload = do
  (State chan _) <- asks getter
  let msg = newMsg {msgBody = encode payload}
  liftIO . void $ publishMsg chan exchange routingKey msg


consumeAndProcess :: (KatipContext m, FromJSON a, MonadCatch m) => Message -> (a -> m Bool) -> m Bool
consumeAndProcess msg handler =
  case eitherDecode' (msgBody msg) of
    Left err -> withMsgAndErr msg err $ do
      $(logTM) ErrorS "Malformed payload. Rejecting."
      pure False
    Right payload -> do
      result <- tryAny (handler payload)
      case result of
        Left err -> withMsgAndErr msg (displayException err) $ do
          $(logTM) ErrorS "Therre was an exception when processing the msg. Rejecting."
          pure False
        Right bool -> pure bool


withMsgAndErr :: (KatipContext m, ToJSON e) => Message -> e -> m a -> m a
withMsgAndErr msg err = katipAddContext (sl "mqMsg" (show msg) <> sl "error" err)