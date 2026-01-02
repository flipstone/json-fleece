{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.FieldTestCases.RequiredField
  ( RequiredField(..)
  , requiredFieldSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype RequiredField = RequiredField T.Text
  deriving (Show, Eq)

requiredFieldSchema :: FC.Fleece t => FC.Schema t RequiredField
requiredFieldSchema =
  FC.coerceSchema FC.text