{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.InlineNoAdditionalPropertiesWithOneOfNullabilityOutside
  ( InlineNoAdditionalPropertiesWithOneOfNullabilityOutside(..)
  , inlineNoAdditionalPropertiesWithOneOfNullabilityOutsideSchema
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Either, Eq, Show)
import qualified TestCases.Types.SingletonNullableOneOfRef as SingletonNullableOneOfRef

newtype InlineNoAdditionalPropertiesWithOneOfNullabilityOutside = InlineNoAdditionalPropertiesWithOneOfNullabilityOutside (Map.Map T.Text (Either FC.Null SingletonNullableOneOfRef.SingletonNullableOneOfRef))
  deriving (Show, Eq)

inlineNoAdditionalPropertiesWithOneOfNullabilityOutsideSchema :: FC.Fleece t => FC.Schema t InlineNoAdditionalPropertiesWithOneOfNullabilityOutside
inlineNoAdditionalPropertiesWithOneOfNullabilityOutsideSchema =
  FC.coerceSchema (FC.map (FC.nullable SingletonNullableOneOfRef.singletonNullableOneOfRefSchema))