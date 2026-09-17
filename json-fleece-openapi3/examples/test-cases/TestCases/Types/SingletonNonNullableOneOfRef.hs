{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SingletonNonNullableOneOfRef
  ( SingletonNonNullableOneOfRef
  , singletonNonNullableOneOfRefSchema
  ) where

import qualified Fleece.Core as FC
import qualified TestCases.Types.AStringType as AStringType

type SingletonNonNullableOneOfRef = AStringType.AStringType

singletonNonNullableOneOfRefSchema :: FC.Fleece t => FC.Schema t SingletonNonNullableOneOfRef
singletonNonNullableOneOfRefSchema =
  FC.coerceSchemaNamed
    (FC.qualifiedName "TestCases.Types.SingletonNonNullableOneOfRef" "SingletonNonNullableOneOfRef")
    AStringType.aStringTypeSchema