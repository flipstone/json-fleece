{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.InlineAllOf.Response200Body
  ( Response200Body(..)
  , response200BodySchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Either, Eq, Maybe, Show)
import qualified TestCases.Operations.InlineAllOf.Response200Body.FieldAItem as FieldAItem
import qualified TestCases.Operations.InlineAllOf.Response200Body.FieldB as FieldB
import qualified TestCases.Operations.InlineAllOf.Response200Body.FieldC as FieldC
import qualified TestCases.Operations.InlineAllOf.Response200Body.FieldD as FieldD
import qualified TestCases.Operations.InlineAllOf.Response200Body.FieldE as FieldE
import qualified TestCases.Operations.InlineAllOf.Response200Body.FieldF as FieldF
import qualified TestCases.Operations.InlineAllOf.Response200Body.FieldH as FieldH
import qualified TestCases.Operations.InlineAllOf.Response200Body.FieldK as FieldK
import qualified TestCases.Operations.InlineAllOf.Response200Body.FieldL as FieldL
import qualified TestCases.Operations.InlineAllOf.Response200Body.FieldM as FieldM
import qualified TestCases.Types.AnyOf as AnyOf
import qualified TestCases.Types.SingletonNullableOneOfRef as SingletonNullableOneOfRef
import qualified TestCases.Types.UTCDateTime as UTCDateTime

data Response200Body = Response200Body
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
  , fieldL :: FieldL.FieldL
  , fieldM :: Maybe FieldM.FieldM
  }
  deriving (Eq, Show)

response200BodySchema :: FC.Fleece schema => schema Response200Body
response200BodySchema =
  FC.object $
    FC.constructor Response200Body
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
      #+ FC.required "fieldL" fieldL FieldL.fieldLSchema
      #+ FC.optional "fieldM" fieldM FieldM.fieldMSchema