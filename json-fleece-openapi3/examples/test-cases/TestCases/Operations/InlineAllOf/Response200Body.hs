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
import qualified TestCases.Operations.InlineAllOf.Response200Body.FieldI as FieldI
import qualified TestCases.Operations.InlineAllOf.Response200Body.FieldK as FieldK
import qualified TestCases.Operations.InlineAllOf.Response200Body.FieldL as FieldL
import qualified TestCases.Operations.InlineAllOf.Response200Body.FieldM as FieldM
import qualified TestCases.Operations.InlineAllOf.Response200Body.FieldN as FieldN
import qualified TestCases.Types.AnyOf as AnyOf
import qualified TestCases.Types.SingletonNullableOneOfRef as SingletonNullableOneOfRef

data Response200Body = Response200Body
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
  , fieldL :: Maybe FieldL.FieldL
  , fieldM :: FieldM.FieldM
  , fieldN :: Maybe FieldN.FieldN
  }
  deriving (Eq, Show)

response200BodySchema :: FC.Fleece t => FC.Schema t Response200Body
response200BodySchema =
  FC.object $
    FC.constructor Response200Body
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
      #+ FC.optional "fieldL" fieldL FieldL.fieldLSchema
      #+ FC.required "fieldM" fieldM FieldM.fieldMSchema
      #+ FC.optional "fieldN" fieldN FieldN.fieldNSchema