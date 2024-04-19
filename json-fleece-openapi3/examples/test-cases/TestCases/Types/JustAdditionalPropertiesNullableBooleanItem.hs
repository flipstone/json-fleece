{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.JustAdditionalPropertiesNullableBooleanItem
  ( JustAdditionalPropertiesNullableBooleanItem(..)
  , justAdditionalPropertiesNullableBooleanItemSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype JustAdditionalPropertiesNullableBooleanItem = JustAdditionalPropertiesNullableBooleanItem Bool
  deriving (Show, Eq)

justAdditionalPropertiesNullableBooleanItemSchema :: FC.Fleece schema => schema JustAdditionalPropertiesNullableBooleanItem
justAdditionalPropertiesNullableBooleanItemSchema =
  FC.coerceSchema FC.boolean