{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.N2_SchemaStartingWithNumber
  ( N2_SchemaStartingWithNumber(..)
  , n2SchemaStartingWithNumberSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype N2_SchemaStartingWithNumber = N2_SchemaStartingWithNumber T.Text
  deriving (Show, Eq)

n2SchemaStartingWithNumberSchema :: FC.Fleece schema => schema N2_SchemaStartingWithNumber
n2SchemaStartingWithNumberSchema =
  FC.coerceSchema FC.text