{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.OneOfWithInlineEnum.Option1
  ( Option1(..)
  , option1Schema
  , option1ToText
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

data Option1
  = One
  | Two
  | Three
  deriving (Eq, Show, Ord, Enum, Bounded)

option1ToText :: Option1 -> T.Text
option1ToText v =
  T.pack $
    case v of
      One -> "one"
      Two -> "two"
      Three -> "three"

option1Schema :: FC.Fleece t => FC.Schema t Option1
option1Schema =
  FC.boundedEnum option1ToText