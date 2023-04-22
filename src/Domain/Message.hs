module Domain.Message where
import Domain.User (User(User))
import Data.Time (UTCTime)

data MessageContent

data Message = Message {fromUser :: User, toUser :: User, time :: UTCTime, content :: MessageContent}

data Dialog = Dialog {owner :: User, participant :: User}

