module Domain.Validation
  ( Validation
  , validate
  , rangeBetween
  , lengthBetween
  , regexMatches
  ) where

import Reexport
import qualified ClassyPrelude as CP
import Text.Regex.PCRE.Heavy


type Validation e a = a -> Maybe e

validate :: (a -> b) -> [Validation e a] -> a -> Either [e] b
validate constructor validations val =
  case concatMap (\f -> maybeToList $ f val) validations of
    [] -> Right (constructor val)
    errs -> Left errs

rangeBetween :: Ord a => a -> a -> e -> Validation e a
rangeBetween minRange maxRange msg = \val ->
  if val >= minRange && val <= maxRange 
    then Nothing
    else Just msg

lengthBetween :: MonoFoldable a => Int -> Int -> e -> a -> Maybe e
lengthBetween minLen maxLen msg = \val -> 
  rangeBetween minLen maxLen msg (CP.length val)


regexMatches :: Regex -> e -> Validation e Text
regexMatches regex msg val = 
  if val =~ regex then Nothing else Just msg