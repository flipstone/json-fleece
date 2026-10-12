{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfPropertyRefNarrowedToUnionMember
  ( AllOfPropertyRefNarrowedToUnionMember(..)
  , allOfPropertyRefNarrowedToUnionMemberSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.AStringType as AStringType

newtype AllOfPropertyRefNarrowedToUnionMember = AllOfPropertyRefNarrowedToUnionMember
  { value :: Maybe AStringType.AStringType -- ^ An explicit type that is just a string for use in other test cases
  }
  deriving (Eq, Show)

allOfPropertyRefNarrowedToUnionMemberSchema :: FC.Fleece t => FC.Schema t AllOfPropertyRefNarrowedToUnionMember
allOfPropertyRefNarrowedToUnionMemberSchema =
  FC.object $
    FC.constructor AllOfPropertyRefNarrowedToUnionMember
      #+ FC.optional "value" value AStringType.aStringTypeSchema