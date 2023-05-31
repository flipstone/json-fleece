{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MixedInJustAdditionalPropertiesSchemaRef.Foo
  ( Foo(..)
  , fooSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Foo = Foo T.Text
  deriving (Show, Eq)

fooSchema :: FC.Fleece schema => schema Foo
fooSchema =
  FC.coerceSchema FC.text