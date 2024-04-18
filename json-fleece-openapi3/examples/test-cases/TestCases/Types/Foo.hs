{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.Foo
  ( Foo(..)
  , fooSchema
  ) where

import Fleece.Core (Object)
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

data Foo = Foo
  deriving (Eq, Show)

fooSchema :: FC.Fleece schema => Object schema Foo Foo
fooSchema =
  FC.constructor Foo