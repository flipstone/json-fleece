{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.ArrayWithSingletonOneOf
  ( ArrayWithSingletonOneOf(..)
  , arrayWithSingletonOneOfSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import qualified TestCases.Types.Foo as Foo

newtype ArrayWithSingletonOneOf = ArrayWithSingletonOneOf [Foo.Foo]
  deriving (Show, Eq)

arrayWithSingletonOneOfSchema :: FC.Fleece t => FC.Schema t ArrayWithSingletonOneOf
arrayWithSingletonOneOfSchema =
  FC.coerceSchema (FC.list Foo.fooSchema)