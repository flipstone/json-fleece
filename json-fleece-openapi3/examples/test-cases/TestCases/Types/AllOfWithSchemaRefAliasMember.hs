{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfWithSchemaRefAliasMember
  ( AllOfWithSchemaRefAliasMember(..)
  , allOfWithSchemaRefAliasMemberSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.AllOfWithSchemaRefAliasMember.ExtraField as ExtraField
import qualified TestCases.Types.TopLevelOneOfOneOption as TopLevelOneOfOneOption

data AllOfWithSchemaRefAliasMember = AllOfWithSchemaRefAliasMember
  { extraField :: Maybe ExtraField.ExtraField
  , oneOfRef :: Maybe TopLevelOneOfOneOption.TopLevelOneOfOneOption
  }
  deriving (Eq, Show)

allOfWithSchemaRefAliasMemberSchema :: FC.Fleece t => FC.Schema t AllOfWithSchemaRefAliasMember
allOfWithSchemaRefAliasMemberSchema =
  FC.object $
    FC.constructor AllOfWithSchemaRefAliasMember
      #+ FC.optional "extraField" extraField ExtraField.extraFieldSchema
      #+ FC.optional "oneOfRef" oneOfRef TopLevelOneOfOneOption.topLevelOneOfOneOptionSchema