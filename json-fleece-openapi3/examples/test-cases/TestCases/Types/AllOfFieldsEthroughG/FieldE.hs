{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfFieldsEthroughG.FieldE
  ( FieldE(..)
  , fieldESchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FieldE = FieldE T.Text
  deriving (Show, Eq)

fieldESchema :: FC.Fleece schema => schema FieldE
fieldESchema =
  FC.coerceSchema FC.text