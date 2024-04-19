{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.JustAdditionalPropertiesNullableBoolean
  ( JustAdditionalPropertiesNullableBoolean(..)
  , justAdditionalPropertiesNullableBooleanSchema
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import qualified TestCases.Types.JustAdditionalPropertiesNullableBooleanItem as JustAdditionalPropertiesNullableBooleanItem

newtype JustAdditionalPropertiesNullableBoolean = JustAdditionalPropertiesNullableBoolean (Map.Map T.Text JustAdditionalPropertiesNullableBooleanItem.JustAdditionalPropertiesNullableBooleanItem)
  deriving (Show, Eq)

justAdditionalPropertiesNullableBooleanSchema :: FC.Fleece schema => schema JustAdditionalPropertiesNullableBoolean
justAdditionalPropertiesNullableBooleanSchema =
  FC.coerceSchema (FC.map JustAdditionalPropertiesNullableBooleanItem.justAdditionalPropertiesNullableBooleanItemSchema)