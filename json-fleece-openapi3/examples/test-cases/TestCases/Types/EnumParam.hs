{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.EnumParam
  ( EnumParam(..)
  , enumParamSchema
  , enumParamToText
  , enumParamFromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Either, Enum, Eq, Ord, Show, String)

data EnumParam
  = Foo
  | Bar
  | Baz
  deriving (Eq, Show, Ord, Enum, Bounded)

enumParamToText :: EnumParam -> T.Text
enumParamToText v =
  T.pack $
    case v of
      Foo -> "foo"
      Bar -> "bar"
      Baz -> "baz"

enumParamFromText :: T.Text -> Either String EnumParam
enumParamFromText txt =
  case T.unpack txt of
    "foo" -> Either.Right Foo
    "bar" -> Either.Right Bar
    "baz" -> Either.Right Baz
    v -> Either.Left $ "Unknown EnumParam: " <> v

enumParamSchema :: FC.Fleece t => FC.Schema t EnumParam
enumParamSchema =
  FC.boundedEnum enumParamToText