{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.EnumParam
  ( EnumParam(..)
  , enumParamSchema
  , enumParamToText
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

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

enumParamSchema :: FC.Fleece t => FC.Schema t EnumParam
enumParamSchema =
  FC.boundedEnum enumParamToText