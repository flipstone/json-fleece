{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.ArrayWithNullableSingletonOneOf
  ( ArrayWithNullableSingletonOneOf(..)
  , arrayWithNullableSingletonOneOfSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Either, Eq, Show)
import qualified TestCases.Types.Foo as Foo

newtype ArrayWithNullableSingletonOneOf = ArrayWithNullableSingletonOneOf [Either FC.Null Foo.Foo]
  deriving (Show, Eq)

arrayWithNullableSingletonOneOfSchema :: FC.Fleece t => FC.Schema t ArrayWithNullableSingletonOneOf
arrayWithNullableSingletonOneOfSchema =
  FC.coerceSchema (FC.list (FC.nullable Foo.fooSchema))