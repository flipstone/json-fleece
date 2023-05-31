{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MixedInJustAdditionalPropertiesSchemaRef.Bar
  ( Bar(..)
  , barSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Bar = Bar T.Text
  deriving (Show, Eq)

barSchema :: FC.Fleece schema => schema Bar
barSchema =
  FC.coerceSchema FC.text