{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfPropertyRefsToSameAliasTargetAliasFirst
  ( AllOfPropertyRefsToSameAliasTargetAliasFirst(..)
  , allOfPropertyRefsToSameAliasTargetAliasFirstSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.SchemaRefAliasOfAlias as SchemaRefAliasOfAlias

newtype AllOfPropertyRefsToSameAliasTargetAliasFirst = AllOfPropertyRefsToSameAliasTargetAliasFirst
  { value :: Maybe SchemaRefAliasOfAlias.SchemaRefAliasOfAlias
  }
  deriving (Eq, Show)

allOfPropertyRefsToSameAliasTargetAliasFirstSchema :: FC.Fleece t => FC.Schema t AllOfPropertyRefsToSameAliasTargetAliasFirst
allOfPropertyRefsToSameAliasTargetAliasFirstSchema =
  FC.object $
    FC.constructor AllOfPropertyRefsToSameAliasTargetAliasFirst
      #+ FC.optional "value" value SchemaRefAliasOfAlias.schemaRefAliasOfAliasSchema