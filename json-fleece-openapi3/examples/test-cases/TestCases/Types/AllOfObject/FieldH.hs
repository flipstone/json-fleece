{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfObject.FieldH
  ( FieldH(..)
  , fieldHSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FieldH = FieldH FC.Null
  deriving (Show, Eq)

fieldHSchema :: FC.Fleece schema => schema FieldH
fieldHSchema =
  FC.coerceSchema FC.null