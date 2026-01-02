{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.OneOfWithInlineObject.Option4.Nested.Number
  ( Number(..)
  , numberSchema
  ) where

import qualified Data.Scientific as Sci
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Number = Number Sci.Scientific
  deriving (Show, Eq)

numberSchema :: FC.Fleece t => FC.Schema t Number
numberSchema =
  FC.coerceSchema FC.number