{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MixedInJustAdditionalPropertiesSchemaInline.Foo
  ( Foo(..)
  , fooSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Foo = Foo T.Text
  deriving (Show, Eq)

fooSchema :: FC.Fleece t => FC.Schema t Foo
fooSchema =
  FC.coerceSchema FC.text