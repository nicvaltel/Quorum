{-# LANGUAGE OverloadedStrings #-}


module Domain.Crypto where

import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import qualified Data.ByteString.Char8 as B

-- >>> let bcryptHash = B.pack "$2a$10$MJJifxfaqQmbx1Mhsq3oq.YmMmfNhkyW4s/MS3K5rIMVfB7w0Q/OW"
-- >>> let password = B.pack "password"
-- >>> validatePassword password bcryptHash
-- >>> True
-- >>> let otherPassword = B.pack "otherpassword"
-- >>> otherHash <- hashPassword 12 otherPasssword :: IO B.ByteString
-- >>> validatePassword otherPassword otherHash
-- >>> True