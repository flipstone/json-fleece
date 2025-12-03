{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.FieldL1
  ( FieldL1(..)
  , fieldL1Schema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.Status as Status

data FieldL1 = FieldL1
  { status :: Maybe Status.Status
  }
  deriving (Eq, Show)

fieldL1Schema :: FC.Fleece schema => schema FieldL1
fieldL1Schema =
  FC.object $
    FC.constructor FieldL1
      #+ FC.optional "status" status Status.statusSchema