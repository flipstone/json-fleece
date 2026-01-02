{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SingletonNullableOneOfPrimitive
  ( SingletonNullableOneOfPrimitive(..)
  , singletonNullableOneOfPrimitiveSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype SingletonNullableOneOfPrimitive = SingletonNullableOneOfPrimitive Bool
  deriving (Show, Eq)

singletonNullableOneOfPrimitiveSchema :: FC.Fleece t => FC.Schema t SingletonNullableOneOfPrimitive
singletonNullableOneOfPrimitiveSchema =
  FC.coerceSchema FC.boolean