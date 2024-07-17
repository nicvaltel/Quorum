module Domain.Posts 
  ( ArticleId
  , Article(..)
  , ArticleError(..)
  ) where

import Reexport

type ArticleId = Int

data Article = Article 
  { articleHead :: Text
  , articleBody :: Text
  } deriving (Show)


data ArticleError = 
    ArticleErrorSessionInactive
  | ArticleErrorUnableToPost
  deriving (Show, Eq, Ord)