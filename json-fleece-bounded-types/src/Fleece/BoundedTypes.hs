{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Additional Fleece schemas for bounded types from external libraries,
including non-empty sets and bounded-length text.
-}
module Fleece.BoundedTypes
  ( nonEmptySet
  , boundedText
  ) where

import qualified Data.BoundedText as BT
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Text as T
import GHC.TypeLits (KnownNat)

import qualified Fleece.Core as FC

{- | Creates a schema for a 'NESet.NESet' (non-empty set) from an array schema,
validating that the resulting set is non-empty. Uses the provided duplicate
handling strategy.
-}
nonEmptySet :: (Ord a, FC.Fleece t) => FC.SetDuplicateHandling -> FC.Schema t a -> FC.Schema t (NESet.NESet a)
nonEmptySet duplicateHandling itemSchema =
  let
    validateNonEmpty items =
      case NESet.nonEmptySet items of
        Just neSet -> Right neSet
        Nothing -> Left "Expected non-empty set, but input was empty"
  in
    FC.minItems 1 $
      FC.validateAnonymous
        NESet.toSet
        validateNonEmpty
        (FC.set duplicateHandling itemSchema)

{- | Creates a schema for 'BT.BoundedText' with type-level minimum and maximum
length bounds. Validates that input text falls within the specified length
range.
-}
boundedText ::
  forall minLen maxLen t.
  (KnownNat minLen, KnownNat maxLen, FC.Fleece t) =>
  FC.Schema t (BT.BoundedText minLen maxLen)
boundedText =
  let
    minVal = toInteger $ BT.boundedTextMinLength @(BT.BoundedText minLen maxLen)
    maxVal = toInteger $ BT.boundedTextMaxLength @(BT.BoundedText minLen maxLen)

    validateBounded :: T.Text -> Either String (BT.BoundedText minLen maxLen)
    validateBounded value =
      case BT.boundedTextFromText value of
        Right bt -> Right bt
        Left err -> Left $ BT.describeBoundedTextError err
  in
    FC.minLength minVal $
      FC.maxLength maxVal $
        FC.validateAnonymous
          BT.boundedTextToText
          validateBounded
          FC.text
