{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.InlineAllOf.Response200Body.FieldC
  ( FieldC(..)
  , fieldCSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.InlineAllOf.Response200Body.FieldC.FieldC1 as FieldC1
import qualified TestCases.Types.InlineAllOf.Response200Body.FieldC.FieldC2 as FieldC2

data FieldC = FieldC
  { fieldC1 :: Maybe FieldC1.FieldC1
  , fieldC2 :: Maybe FieldC2.FieldC2
  }
  deriving (Eq, Show)

fieldCSchema :: FC.Fleece t => FC.Schema t FieldC
fieldCSchema =
  FC.object $
    FC.constructor FieldC
      #+ FC.optional "fieldC1" fieldC1 FieldC1.fieldC1Schema
      #+ FC.optional "fieldC2" fieldC2 FieldC2.fieldC2Schema