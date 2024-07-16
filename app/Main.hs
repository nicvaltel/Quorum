module Main (main) where

import Prelude(IO)
import qualified WebSocketServer as W
import qualified Lib
import qualified Logging
import qualified Adapter.Redis.Auth as R
import qualified Adapter.HTTP.Main as H

main :: IO ()
main = do
		 Lib.runRoutine


  -- R.runExample
  -- Logging.runKatip
  -- W.main
  -- H.main