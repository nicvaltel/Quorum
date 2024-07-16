{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.Main where


import Reexport
import Web.Scotty.Trans
import ClassyPrelude (LText, MonadUnliftIO)
import Web.Scotty.Trans (ScottyException)



main :: IO ()
main = scottyT 3000 id routes

-- routes :: (MonadUnliftIO m) => ScottyT m ()
-- routes = get "/hello" $ text "Hello"

routes :: (MonadUnliftIO m) => ScottyT m ()
routes = do
  get "/" $ text "home"
  get "/hello/:name" $ do
    name <- param ":name"
    -- name <- catch (captureParam ":name") (\(e :: ScottyException) -> pure "anonymous")
    text $ "Hello, " <> name
  post "/users" $ text "adding user"
  put "/users/:id" $ text "updating user"
  patch "/users/:id" $ text "partially updating users"
  delete "/users/:id" $ text "deleting user"
  matchAny "/admin" $ text "I don't care about your HTTP verb"
  options (regex ".*") $ text "CORS usually use this"
  notFound $ text "404"