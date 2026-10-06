{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.SchemaRefAliasOrNullableBoolean
  ( SchemaRefAliasOrNullableBoolean(..)
  , schemaRefAliasOrNullableBooleanSchema
  ) where

import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Either, Eq, Show)
import qualified Shrubbery as Shrubbery
import qualified TestCases.Types.NullableBoolean as NullableBoolean
import qualified TestCases.Types.SchemaRefAlias as SchemaRefAlias

newtype SchemaRefAliasOrNullableBoolean = SchemaRefAliasOrNullableBoolean (Shrubbery.Union
  '[ SchemaRefAlias.SchemaRefAlias
   , Either FC.Null NullableBoolean.NullableBoolean
   ])
  deriving (Show, Eq)

schemaRefAliasOrNullableBooleanSchema :: FC.Fleece t => FC.Schema t SchemaRefAliasOrNullableBoolean
schemaRefAliasOrNullableBooleanSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.SchemaRefAliasOrNullableBoolean" "SchemaRefAliasOrNullableBoolean") $
      FC.unionMember SchemaRefAlias.schemaRefAliasSchema
        #| FC.unionMember (FC.nullable NullableBoolean.nullableBooleanSchema)