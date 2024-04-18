{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.Baz
  ( Baz(..)
  , bazSchema
  ) where

import Fleece.Core ((#+), Object)
import qualified Fleece.Core as FC
import Prelude (Eq, Maybe, Show)
import qualified TestCases.Types.Baz.BazName as BazName

data Baz = Baz
  { bazName :: Maybe BazName.BazName
  }
  deriving (Eq, Show)

bazSchema :: FC.Fleece schema => Object schema Baz Baz
bazSchema =
  FC.constructor Baz
    #+ FC.optional "bazName" bazName BazName.bazNameSchema