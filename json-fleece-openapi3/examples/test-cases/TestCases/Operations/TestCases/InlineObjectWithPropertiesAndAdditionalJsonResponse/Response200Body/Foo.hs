{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.TestCases.InlineObjectWithPropertiesAndAdditionalJsonResponse.Response200Body.Foo
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