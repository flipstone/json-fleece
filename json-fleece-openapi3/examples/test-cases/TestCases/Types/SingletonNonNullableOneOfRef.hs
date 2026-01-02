{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SingletonNonNullableOneOfRef
  ( SingletonNonNullableOneOfRef(..)
  , singletonNonNullableOneOfRefSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype SingletonNonNullableOneOfRef = SingletonNonNullableOneOfRef T.Text
  deriving (Show, Eq)

singletonNonNullableOneOfRefSchema :: FC.Fleece t => FC.Schema t SingletonNonNullableOneOfRef
singletonNonNullableOneOfRefSchema =
  FC.coerceSchema FC.text