{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.JustAdditionalPropertiesSchemaRef
  ( JustAdditionalPropertiesSchemaRef(..)
  , justAdditionalPropertiesSchemaRefSchema
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import qualified TestCases.Types.AStringType as AStringType

newtype JustAdditionalPropertiesSchemaRef = JustAdditionalPropertiesSchemaRef (Map.Map T.Text AStringType.AStringType)
  deriving (Show, Eq)

justAdditionalPropertiesSchemaRefSchema :: FC.Fleece schema => schema JustAdditionalPropertiesSchemaRef
justAdditionalPropertiesSchemaRefSchema =
  FC.coerceSchema (FC.map AStringType.aStringTypeSchema)