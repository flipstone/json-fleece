{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.JustAdditionalPropertiesNullableBooleanRef
  ( JustAdditionalPropertiesNullableBooleanRef(..)
  , justAdditionalPropertiesNullableBooleanRefSchema
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Either, Eq, Show)
import qualified TestCases.Types.NullableBoolean as NullableBoolean

newtype JustAdditionalPropertiesNullableBooleanRef = JustAdditionalPropertiesNullableBooleanRef (Map.Map T.Text (Either FC.Null NullableBoolean.NullableBoolean))
  deriving (Show, Eq)

justAdditionalPropertiesNullableBooleanRefSchema :: FC.Fleece schema => schema JustAdditionalPropertiesNullableBooleanRef
justAdditionalPropertiesNullableBooleanRefSchema =
  FC.coerceSchema (FC.map (FC.nullable NullableBoolean.nullableBooleanSchema))