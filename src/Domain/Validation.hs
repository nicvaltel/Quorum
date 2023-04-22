module Domain.Validation 
(
  validate,
  regexMatches,
  lengthBetween,
  rangeBetween
) where


import Text.Regex.PCRE.Heavy
import Data.Text (Text)
import Data.Maybe (catMaybes)
import GHC.Exts (IsList (toList))

type Validataion e a = a -> Maybe e

validate :: (a -> b) -> [Validataion e a] -> a -> Either [e] b
validate constructor validations val =
  case catMaybes [f val | f <- validations] of
    [] -> Right (constructor val)
    errs -> Left errs


rangeBetween :: (Ord a) => a -> a -> e -> Validataion e a
rangeBetween minRange maxRange msg val
  | val >= minRange  && val <= maxRange = Nothing
  | otherwise = Just msg

lengthBetween :: (IsList a) => Int -> Int -> e -> Validataion e a
lengthBetween minLen maxLen msg val = rangeBetween minLen maxLen msg (length $ toList val)


regexMatches :: Regex -> e -> Validataion e Text
regexMatches regex msg val
  | val =~ regex = Nothing
  | otherwise = Just msg