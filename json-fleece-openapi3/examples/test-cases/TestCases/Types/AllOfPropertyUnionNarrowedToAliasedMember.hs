{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfPropertyUnionNarrowedToAliasedMember
  ( AllOfPropertyUnionNarrowedToAliasedMember(..)
  , allOfPropertyUnionNarrowedToAliasedMemberSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.AStringType as AStringType

newtype AllOfPropertyUnionNarrowedToAliasedMember = AllOfPropertyUnionNarrowedToAliasedMember
  { value :: Maybe AStringType.AStringType -- ^ An explicit type that is just a string for use in other test cases
  }
  deriving (Eq, Show)

allOfPropertyUnionNarrowedToAliasedMemberSchema :: FC.Fleece t => FC.Schema t AllOfPropertyUnionNarrowedToAliasedMember
allOfPropertyUnionNarrowedToAliasedMemberSchema =
  FC.object $
    FC.constructor AllOfPropertyUnionNarrowedToAliasedMember
      #+ FC.optional "value" value AStringType.aStringTypeSchema