{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfObject
  ( AllOfObject(..)
  , allOfObjectSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Either, Eq, Maybe, Show)
import qualified TestCases.Types.AllOfObject.FieldAItem as FieldAItem
import qualified TestCases.Types.AllOfObject.FieldB as FieldB
import qualified TestCases.Types.AllOfObject.FieldC as FieldC
import qualified TestCases.Types.AllOfObject.FieldD as FieldD
import qualified TestCases.Types.AllOfObject.FieldE as FieldE
import qualified TestCases.Types.AllOfObject.FieldF as FieldF
import qualified TestCases.Types.AllOfObject.FieldH as FieldH
import qualified TestCases.Types.AllOfObject.FieldI as FieldI
import qualified TestCases.Types.AllOfObject.FieldK as FieldK
import qualified TestCases.Types.AnyOf as AnyOf
import qualified TestCases.Types.SingletonNullableOneOfRef as SingletonNullableOneOfRef

data AllOfObject = AllOfObject
  { fieldA :: [FieldAItem.FieldAItem]
  , fieldB :: Either FC.Null FieldB.FieldB -- ^ A nullable date-time.
  , fieldC :: FieldC.FieldC
  , fieldD :: FieldD.FieldD
  , fieldE :: Either FC.Null FieldE.FieldE -- ^ This is also defined in AllOfFieldsEthroughG
  , fieldF :: Maybe FieldF.FieldF
  , fieldG :: Maybe (Either FC.Null SingletonNullableOneOfRef.SingletonNullableOneOfRef)
  , fieldH :: Maybe FieldH.FieldH
  , fieldI :: Maybe FieldI.FieldI -- ^ Some description.
  , fieldJ :: Maybe AnyOf.AnyOf
  , fieldK :: FieldK.FieldK -- ^ Field K.
  }
  deriving (Eq, Show)

allOfObjectSchema :: FC.Fleece schema => schema AllOfObject
allOfObjectSchema =
  FC.object $
    FC.constructor AllOfObject
      #+ FC.required "fieldA" fieldA (FC.list FieldAItem.fieldAItemSchema)
      #+ FC.required "fieldB" fieldB (FC.nullable FieldB.fieldBSchema)
      #+ FC.required "fieldC" fieldC FieldC.fieldCSchema
      #+ FC.required "fieldD" fieldD FieldD.fieldDSchema
      #+ FC.required "fieldE" fieldE (FC.nullable FieldE.fieldESchema)
      #+ FC.optional "fieldF" fieldF FieldF.fieldFSchema
      #+ FC.optional "fieldG" fieldG (FC.nullable SingletonNullableOneOfRef.singletonNullableOneOfRefSchema)
      #+ FC.optional "fieldH" fieldH FieldH.fieldHSchema
      #+ FC.optional "fieldI" fieldI FieldI.fieldISchema
      #+ FC.optional "fieldJ" fieldJ AnyOf.anyOfSchema
      #+ FC.required "fieldK" fieldK FieldK.fieldKSchema