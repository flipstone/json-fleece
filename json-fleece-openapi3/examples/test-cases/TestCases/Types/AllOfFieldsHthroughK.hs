{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfFieldsHthroughK
  ( AllOfFieldsHthroughK(..)
  , allOfFieldsHthroughKSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.AllOfFieldsHthroughK.FieldH as FieldH
import qualified TestCases.Types.AllOfFieldsHthroughK.FieldI as FieldI
import qualified TestCases.Types.AllOfFieldsHthroughK.FieldK as FieldK
import qualified TestCases.Types.AnyOf as AnyOf

data AllOfFieldsHthroughK = AllOfFieldsHthroughK
  { fieldH :: Maybe FieldH.FieldH
  , fieldI :: Maybe FieldI.FieldI -- ^ Some description.
  , fieldJ :: Maybe AnyOf.AnyOf
  , fieldK :: Maybe FieldK.FieldK -- ^ Field K.
  }
  deriving (Eq, Show)

allOfFieldsHthroughKSchema :: FC.Fleece t => FC.Schema t AllOfFieldsHthroughK
allOfFieldsHthroughKSchema =
  FC.object $
    FC.constructor AllOfFieldsHthroughK
      #+ FC.optional "fieldH" fieldH FieldH.fieldHSchema
      #+ FC.optional "fieldI" fieldI FieldI.fieldISchema
      #+ FC.optional "fieldJ" fieldJ AnyOf.anyOfSchema
      #+ FC.optional "fieldK" fieldK FieldK.fieldKSchema