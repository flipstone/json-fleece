{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.Foo
  ( Foo(..)
  , fooSchema
  , fooObjSchema
  ) where

import Fleece.Core (Object)
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

data Foo = Foo
  deriving (Eq, Show)

fooSchema :: FC.Fleece schema => schema Foo
fooSchema =
  FC.object fooObjSchema

fooObjSchema :: FC.Fleece schema => Object schema Foo Foo
fooObjSchema =
  FC.constructor Foo