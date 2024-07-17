module Domain.Posts 
  ( ArticleId
  , CommentId
  , Article(..)
  , Comment(..)
  , PostError(..)
  ) where

import Reexport

type ArticleId = Int

type CommentId = Int

data Article = Article 
  { articleHead :: Text
  , articleBody :: Text
  } deriving (Show)

newtype Comment = Comment {commentMessage :: Text}
  deriving (Show)

data PostError = 
    PostErrorSessionInactive
  | CommentPostErrorArticleNotFound
  | PostErrorUserIdNotFound
  deriving (Show, Eq, Ord)