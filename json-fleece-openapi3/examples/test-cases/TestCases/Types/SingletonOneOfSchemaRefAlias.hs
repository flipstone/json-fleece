{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.SingletonOneOfSchemaRefAlias
  ( SingletonOneOfSchemaRefAlias(..)
  , singletonOneOfSchemaRefAliasSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype SingletonOneOfSchemaRefAlias = SingletonOneOfSchemaRefAlias T.Text
  deriving (Show, Eq)

singletonOneOfSchemaRefAliasSchema :: FC.Fleece t => FC.Schema t SingletonOneOfSchemaRefAlias
singletonOneOfSchemaRefAliasSchema =
  FC.coerceSchema FC.text