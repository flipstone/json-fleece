{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SingletonNullableOneOfRef
  ( SingletonNullableOneOfRef
  , singletonNullableOneOfRefSchema
  ) where

import qualified Fleece.Core as FC
import qualified TestCases.Types.AStringType as AStringType

type SingletonNullableOneOfRef = AStringType.AStringType

singletonNullableOneOfRefSchema :: FC.Fleece t => FC.Schema t SingletonNullableOneOfRef
singletonNullableOneOfRefSchema =
  FC.coerceSchemaNamed
    (FC.qualifiedName "TestCases.Types.SingletonNullableOneOfRef" "SingletonNullableOneOfRef")
    AStringType.aStringTypeSchema