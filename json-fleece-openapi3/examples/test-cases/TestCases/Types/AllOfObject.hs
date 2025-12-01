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
import qualified TestCases.Types.AllOfObject.FieldK as FieldK
import qualified TestCases.Types.AnyOf as AnyOf
import qualified TestCases.Types.SingletonNullableOneOfRef as SingletonNullableOneOfRef
import qualified TestCases.Types.UTCDateTime as UTCDateTime

data AllOfObject = AllOfObject
  { fieldA :: [FieldAItem.FieldAItem]
  , fieldB :: FieldB.FieldB
  , fieldC :: FieldC.FieldC
  , fieldD :: FieldD.FieldD
  , fieldE :: Either FC.Null FieldE.FieldE
  , fieldF :: Maybe FieldF.FieldF
  , fieldG :: Maybe (Either FC.Null SingletonNullableOneOfRef.SingletonNullableOneOfRef)
  , fieldH :: Maybe FieldH.FieldH
  , fieldI :: Maybe UTCDateTime.UTCDateTime
  , fieldJ :: Maybe AnyOf.AnyOf
  , fieldK :: FieldK.FieldK -- ^ Field K.
  }
  deriving (Eq, Show)

allOfObjectSchema :: FC.Fleece schema => schema AllOfObject
allOfObjectSchema =
  FC.object $
    FC.constructor AllOfObject
      #+ FC.required "fieldA" fieldA (FC.list FieldAItem.fieldAItemSchema)
      #+ FC.required "fieldB" fieldB FieldB.fieldBSchema
      #+ FC.required "fieldC" fieldC FieldC.fieldCSchema
      #+ FC.required "fieldD" fieldD FieldD.fieldDSchema
      #+ FC.required "fieldE" fieldE (FC.nullable FieldE.fieldESchema)
      #+ FC.optional "fieldF" fieldF FieldF.fieldFSchema
      #+ FC.optional "fieldG" fieldG (FC.nullable SingletonNullableOneOfRef.singletonNullableOneOfRefSchema)
      #+ FC.optional "fieldH" fieldH FieldH.fieldHSchema
      #+ FC.optional "fieldI" fieldI UTCDateTime.uTCDateTimeSchema
      #+ FC.optional "fieldJ" fieldJ AnyOf.anyOfSchema
      #+ FC.required "fieldK" fieldK FieldK.fieldKSchema