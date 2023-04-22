module Adapter.Repository where
import Domain.User (User(User), Email)
import System.Posix.Types (UserID)
import Control.Monad.Cont (MonadIO)



class MonadIO m => UsersRepo m where
  allUsers :: m [User]
  findUserById :: UserID -> m (Maybe User)
  findUserByEmail :: Email -> m (Maybe User)
  addUser :: User -> m ()
  deleteUser :: User -> m Bool