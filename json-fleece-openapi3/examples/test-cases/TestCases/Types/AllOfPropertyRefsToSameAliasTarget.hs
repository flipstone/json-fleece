{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfPropertyRefsToSameAliasTarget
  ( AllOfPropertyRefsToSameAliasTarget(..)
  , allOfPropertyRefsToSameAliasTargetSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.AStringType as AStringType

newtype AllOfPropertyRefsToSameAliasTarget = AllOfPropertyRefsToSameAliasTarget
  { value :: Maybe AStringType.AStringType -- ^ An explicit type that is just a string for use in other test cases
  }
  deriving (Eq, Show)

allOfPropertyRefsToSameAliasTargetSchema :: FC.Fleece t => FC.Schema t AllOfPropertyRefsToSameAliasTarget
allOfPropertyRefsToSameAliasTargetSchema =
  FC.object $
    FC.constructor AllOfPropertyRefsToSameAliasTarget
      #+ FC.optional "value" value AStringType.aStringTypeSchema