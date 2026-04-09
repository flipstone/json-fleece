{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfFieldsEthroughG.FieldE
  ( FieldE(..)
  , fieldESchema
  , fieldEToText
  , fieldEFromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Enum, Eq, Ord, Show, String)

data FieldE
  = Enum1
  | Enum3
  deriving (Eq, Show, Ord, Enum, Bounded)

fieldEToText :: FieldE -> T.Text
fieldEToText v =
  T.pack $
    case v of
      Enum1 -> "enum1"
      Enum3 -> "enum3"

fieldEFromText :: T.Text -> Either.Either String FieldE
fieldEFromText txt =
  case T.unpack txt of
    "enum1" -> Either.Right Enum1
    "enum3" -> Either.Right Enum3
    v -> Either.Left $ "Unknown FieldE: " <> v

fieldESchema :: FC.Fleece t => FC.Schema t FieldE
fieldESchema =
  FC.boundedEnum fieldEToText