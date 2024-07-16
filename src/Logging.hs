{-# LANGUAGE TypeApplications #-}

module Logging (withKatip) where



import Reexport


runKatip :: IO ()
runKatip = withKatip $ \le -> 
  runKatipContextT le () mempty logSomething

withKatip :: (LogEnv -> IO a) -> IO a
withKatip app = bracket createLogEnv closeScribes app
  where
    createLogEnv = do
      logEnv <- initLogEnv "HAuth" "dev"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv


logSomething :: (KatipContext m ) => m ()
logSomething = do
  $(logTM) InfoS "Log in no namespace"
  katipAddNamespace "ns1" $
    $(logTM) InfoS "Log in ns1"
  katipAddNamespace "ns2" $ do
    $(logTM) WarningS "Log in ns2"
    katipAddNamespace "ns3" $
     katipAddContext (sl @Text "userId" "12") $ do
      $(logTM) InfoS "Log in ns2.ns3 with userId context"
      katipAddContext (sl @Text "country" "Singapore") $
        $(logTM) InfoS "Log in ns2.ns3 with userId and country context"



runLogExample :: IO ()
runLogExample = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  let makeLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "MyApp" "production"
  -- closeScribes will stop accepting new logs, flush existing ones and clean up resources
  bracket makeLogEnv closeScribes $ \le -> do
    let initialContext = () -- this context will be attached to every log in your app and merged w/ subsequent contexts
    let initialNamespace = "main"
    runKatipContextT le initialContext initialNamespace $ do
      $(logTM) InfoS "Hello Katip"
      -- This adds a namespace to the current namespace and merges a piece of contextual data into your context
      katipAddNamespace "additional_namespace" $ katipAddContext (sl "some_context" True) $ do
        $(logTM) WarningS "Now we're getting fancy"
      katipNoLogging $ do
        $(logTM) DebugS "You will never see this!"