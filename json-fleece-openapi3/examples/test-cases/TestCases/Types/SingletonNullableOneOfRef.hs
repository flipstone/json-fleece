{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SingletonNullableOneOfRef
  ( SingletonNullableOneOfRef(..)
  , singletonNullableOneOfRefSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype SingletonNullableOneOfRef = SingletonNullableOneOfRef T.Text
  deriving (Show, Eq)

singletonNullableOneOfRefSchema :: FC.Fleece schema => schema SingletonNullableOneOfRef
singletonNullableOneOfRefSchema =
  FC.coerceSchema FC.text