{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.OneOfWithNullable
  ( OneOfWithNullable(..)
  , oneOfWithNullableSchema
  ) where

import qualified Data.Text as T
import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Either, Eq, Integer, Show)
import qualified Shrubbery as Shrubbery
import qualified TestCases.Types.AStringType as AStringType

newtype OneOfWithNullable = OneOfWithNullable (Shrubbery.Union
  '[ Integer
   , Either FC.Null T.Text
   , Either FC.Null [Either FC.Null T.Text]
   , [AStringType.AStringType]
   , Either FC.Null [AStringType.AStringType]
   , [Either FC.Null [AStringType.AStringType]]
  ])
  deriving (Show, Eq)

oneOfWithNullableSchema :: FC.Fleece schema => schema OneOfWithNullable
oneOfWithNullableSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.OneOfWithNullable" "OneOfWithNullable") $
      FC.unionMember FC.integer
        #| FC.unionMember (FC.nullable FC.text)
        #| FC.unionMember (FC.nullable (FC.list (FC.nullable FC.text)))
        #| FC.unionMember (FC.list AStringType.aStringTypeSchema)
        #| FC.unionMember (FC.nullable (FC.list AStringType.aStringTypeSchema))
        #| FC.unionMember (FC.list (FC.nullable (FC.list AStringType.aStringTypeSchema)))