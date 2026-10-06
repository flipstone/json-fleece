{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.AStringOrNullableBoolean
  ( AStringOrNullableBoolean(..)
  , aStringOrNullableBooleanSchema
  ) where

import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Either, Eq, Show)
import qualified Shrubbery as Shrubbery
import qualified TestCases.Types.AStringType as AStringType
import qualified TestCases.Types.NullableBoolean as NullableBoolean

newtype AStringOrNullableBoolean = AStringOrNullableBoolean (Shrubbery.Union
  '[ AStringType.AStringType
   , Either FC.Null NullableBoolean.NullableBoolean
   ])
  deriving (Show, Eq)

aStringOrNullableBooleanSchema :: FC.Fleece t => FC.Schema t AStringOrNullableBoolean
aStringOrNullableBooleanSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.AStringOrNullableBoolean" "AStringOrNullableBoolean") $
      FC.unionMember AStringType.aStringTypeSchema
        #| FC.unionMember (FC.nullable NullableBoolean.nullableBooleanSchema)