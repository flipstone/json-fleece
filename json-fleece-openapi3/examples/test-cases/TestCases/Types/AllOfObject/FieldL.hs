{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfObject.FieldL
  ( FieldL(..)
  , fieldLSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.Status as Status

data FieldL = FieldL
  { status :: Maybe Status.Status
  }
  deriving (Eq, Show)

fieldLSchema :: FC.Fleece schema => schema FieldL
fieldLSchema =
  FC.object $
    FC.constructor FieldL
      #+ FC.optional "status" status Status.statusSchema