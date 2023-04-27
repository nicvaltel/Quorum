module Adapter.InMemory.Repository where

import System.Posix.Types (UserID)
import Domain.User (User)
import Data.HashMap.Strict (HashMap)


newtype UsersRepo = UserRepo (HashMap UserID User)

-- addUser :: User -> 