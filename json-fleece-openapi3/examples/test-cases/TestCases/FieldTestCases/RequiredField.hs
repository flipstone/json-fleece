{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.FieldTestCases.RequiredField
  ( RequiredField(..)
  , requiredFieldSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype RequiredField = RequiredField Text
  deriving (Show, Eq)

requiredFieldSchema :: FC.Fleece schema => schema RequiredField
requiredFieldSchema =
  FC.coerceSchema FC.text