{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.Num2SchemaStartingWithNumber
  ( Num2SchemaStartingWithNumber(..)
  , num2SchemaStartingWithNumberSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Num2SchemaStartingWithNumber = Num2SchemaStartingWithNumber T.Text
  deriving (Show, Eq)

num2SchemaStartingWithNumberSchema :: FC.Fleece schema => schema Num2SchemaStartingWithNumber
num2SchemaStartingWithNumberSchema =
  FC.coerceSchema FC.text