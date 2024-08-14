module Fleece.OpenApi3.Schemas.OpenApi3Validator
  ( OpenApi3Validator (..)
  ) where

import qualified Data.Foldable as Foldable
import Data.Scientific (Scientific)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Fleece.Core as FC

class FC.FleeceValidator validator => OpenApi3Validator validator where
  maximumScientific :: Scientific -> validator Scientific Scientific
  minimumScientific :: Scientific -> validator Scientific Scientific

  maximumIntegral :: Integral a => a -> validator a a
  minimumIntegral :: Integral a => a -> validator a a

  maxLength :: Int -> validator T.Text T.Text
  minLength :: Int -> validator T.Text T.Text

  maxItems :: Int -> validator (V.Vector a) (V.Vector a)
  minItems :: Int -> validator (V.Vector a) (V.Vector a)
  uniqueItems :: Ord a => FC.SetDuplicateHandling -> validator (V.Vector a) (Set.Set a)

  setValidatorType :: T.Text -> validator a b -> validator a b
  setValidatorFormat :: T.Text -> validator a b -> validator a b

instance OpenApi3Validator FC.StandardValidator where
  maximumScientific n =
    FC.mkValidator
      id
      ( \x ->
          if x > n
            then Left ("Value " <> show x <> " is greater than maximum of " <> show n)
            else Right x
      )

  minimumScientific n =
    FC.mkValidator
      id
      ( \x ->
          if x < n
            then Left ("Value " <> show x <> " is less than minimum of " <> show n)
            else Right x
      )

  maximumIntegral n =
    FC.mkValidator
      id
      ( \x ->
          if x > n
            then Left ("Value " <> show (toInteger x) <> " is greater than maximum of " <> show (toInteger n))
            else Right x
      )

  minimumIntegral n =
    FC.mkValidator
      id
      ( \x ->
          if x < n
            then Left ("Value " <> show (toInteger x) <> " is less than minimum of " <> show (toInteger x))
            else Right x
      )

  maxLength n =
    FC.mkValidator
      id
      ( \x ->
          if T.length x > n
            then Left ("Text length " <> show (T.length x) <> " is greater than maximum of " <> show n)
            else Right x
      )

  minLength n =
    FC.mkValidator
      id
      ( \x ->
          if T.length x < n
            then Left ("Text length " <> show (T.length x) <> " is less than minimum of " <> show n)
            else Right x
      )

  maxItems n =
    FC.mkValidator
      id
      ( \xs ->
          if length xs > n
            then Left ("Array length " <> show (length xs) <> " is greater than maximum of " <> show n)
            else Right xs
      )

  minItems n =
    FC.mkValidator
      id
      ( \xs ->
          if length xs < n
            then Left ("Array length " <> show (length xs) <> " is less than minimum of " <> show n)
            else Right xs
      )

  uniqueItems handling =
    FC.mkValidator
      (V.fromList . Set.toList)
      ( \xs ->
          let
            set = Foldable.foldl' (flip Set.insert) Set.empty xs
          in
            case handling of
              FC.AllowInputDuplicates -> Right set
              FC.RejectInputDuplicates
                | length set < length xs -> Left "Unexpected duplicates in input array."
                | otherwise -> Right set
      )

  setValidatorType _ v = v

  setValidatorFormat _ v = v

instance OpenApi3Validator FC.NoOpValidator where
  maximumScientific = const FC.NoOpValidator
  minimumScientific = const FC.NoOpValidator

  maximumIntegral = const FC.NoOpValidator
  minimumIntegral = const FC.NoOpValidator

  maxLength = const FC.NoOpValidator
  minLength = const FC.NoOpValidator

  maxItems = const FC.NoOpValidator
  minItems = const FC.NoOpValidator
  uniqueItems = const FC.NoOpValidator

  setValidatorType _ = const FC.NoOpValidator
  setValidatorFormat _ = const FC.NoOpValidator
