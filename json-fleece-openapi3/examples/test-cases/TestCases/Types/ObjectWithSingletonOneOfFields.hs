{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.ObjectWithSingletonOneOfFields
  ( ObjectWithSingletonOneOfFields(..)
  , objectWithSingletonOneOfFieldsSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Either, Eq, Maybe, Show)
import qualified TestCases.Types.Foo as Foo

data ObjectWithSingletonOneOfFields = ObjectWithSingletonOneOfFields
  { nullableOneOf :: Maybe (Either FC.Null Foo.Foo)
  , oneOf :: Maybe Foo.Foo
  }
  deriving (Eq, Show)

objectWithSingletonOneOfFieldsSchema :: FC.Fleece t => FC.Schema t ObjectWithSingletonOneOfFields
objectWithSingletonOneOfFieldsSchema =
  FC.object $
    FC.constructor ObjectWithSingletonOneOfFields
      #+ FC.optional "nullableOneOf" nullableOneOf (FC.nullable Foo.fooSchema)
      #+ FC.optional "oneOf" oneOf Foo.fooSchema