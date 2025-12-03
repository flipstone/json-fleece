{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.InlineAllOf.Response200Body.FieldE
  ( FieldE(..)
  , fieldESchema
  , fieldEToText
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

data FieldE
  = Enum1
  | Enum2
  | Enum3
  deriving (Eq, Show, Ord, Enum, Bounded)

fieldEToText :: FieldE -> T.Text
fieldEToText v =
  T.pack $
    case v of
      Enum1 -> "enum1"
      Enum2 -> "enum2"
      Enum3 -> "enum3"

fieldESchema :: FC.Fleece schema => schema FieldE
fieldESchema =
  FC.boundedEnum fieldEToText