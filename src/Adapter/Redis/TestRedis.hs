module Adapter.Redis.TestRedis where


import Database.Redis
import Control.Monad.Reader (runReader)

-- checkedConnect :: ConnectInfo -> IO Connection

connInf :: ConnectInfo 
connInf = defaultConnectInfo


runTest :: IO ()
runTest = do
  conn <- checkedConnect connInf
  -- runRedis conn $ set "Hello" "World"
  x <- runRedis conn $ get "Hello"
  print x
  pure ()