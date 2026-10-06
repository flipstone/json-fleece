{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfPropertyUnionNarrowedToMemberRef
  ( AllOfPropertyUnionNarrowedToMemberRef(..)
  , allOfPropertyUnionNarrowedToMemberRefSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.AStringType as AStringType

newtype AllOfPropertyUnionNarrowedToMemberRef = AllOfPropertyUnionNarrowedToMemberRef
  { value :: Maybe AStringType.AStringType -- ^ An explicit type that is just a string for use in other test cases
  }
  deriving (Eq, Show)

allOfPropertyUnionNarrowedToMemberRefSchema :: FC.Fleece t => FC.Schema t AllOfPropertyUnionNarrowedToMemberRef
allOfPropertyUnionNarrowedToMemberRefSchema =
  FC.object $
    FC.constructor AllOfPropertyUnionNarrowedToMemberRef
      #+ FC.optional "value" value AStringType.aStringTypeSchema