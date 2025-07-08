{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SingletonNullableOneOfPrimitive
  ( SingletonNullableOneOfPrimitive(..)
  , singletonNullableOneOfPrimitiveSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SingletonNullableOneOfPrimitive = SingletonNullableOneOfPrimitive Bool
  deriving (Show, Eq)

singletonNullableOneOfPrimitiveSchema :: FC.Fleece schema => schema SingletonNullableOneOfPrimitive
singletonNullableOneOfPrimitiveSchema =
  FC.coerceSchema FC.boolean