module Domain.User
  ( UserId,
    Username,
    Email (..),
    Password (..),
    User (..),
    rawEmail,
    rawPassword,
    mkEmail,
    mkPassword,
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Domain.Validation
import Text.Regex.PCRE.Heavy

type UserId = Int

type Username = Text

newtype Email = Email {emailRaw :: Text} deriving (Show, Eq, Ord)

newtype Password = Password {passwordRaw :: Text} deriving (Show, Eq)

data User = User {userId :: UserId, username :: Username, email :: Email, password :: Password}

rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [Text] Email
mkEmail = \eml -> Right (Email eml) 
-- mkEmail =
--   validate
--     Email
--     [ regexMatches [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|] "Not a valid email"
--     ]

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword = \pwd -> Right (Password pwd) 
-- mkPassword = 
--   validate
--     Password
--     [ lengthBetween 8 50 "Should between 8 and 50",
--       regexMatches [re|\d|] "Should contain number",
--       regexMatches [re|[A-Z]|] "Should contain uppercase letter",
--       regexMatches [re|[a-z]|] "Should contain lowercase letter"
--     ]
