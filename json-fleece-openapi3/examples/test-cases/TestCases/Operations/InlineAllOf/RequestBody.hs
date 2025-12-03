{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.InlineAllOf.RequestBody
  ( RequestBody(..)
  , requestBodySchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Either, Eq, Maybe, Show)
import qualified TestCases.Operations.InlineAllOf.RequestBody.FieldAItem as FieldAItem
import qualified TestCases.Operations.InlineAllOf.RequestBody.FieldB as FieldB
import qualified TestCases.Operations.InlineAllOf.RequestBody.FieldC as FieldC
import qualified TestCases.Operations.InlineAllOf.RequestBody.FieldD as FieldD
import qualified TestCases.Operations.InlineAllOf.RequestBody.FieldE as FieldE
import qualified TestCases.Operations.InlineAllOf.RequestBody.FieldF as FieldF
import qualified TestCases.Operations.InlineAllOf.RequestBody.FieldH as FieldH
import qualified TestCases.Operations.InlineAllOf.RequestBody.FieldI as FieldI
import qualified TestCases.Operations.InlineAllOf.RequestBody.FieldK as FieldK
import qualified TestCases.Operations.InlineAllOf.RequestBody.FieldL as FieldL
import qualified TestCases.Operations.InlineAllOf.RequestBody.FieldM as FieldM
import qualified TestCases.Types.AnyOf as AnyOf
import qualified TestCases.Types.SingletonNullableOneOfRef as SingletonNullableOneOfRef

data RequestBody = RequestBody
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
  , fieldL :: FieldL.FieldL
  , fieldM :: Maybe FieldM.FieldM
  }
  deriving (Eq, Show)

requestBodySchema :: FC.Fleece schema => schema RequestBody
requestBodySchema =
  FC.object $
    FC.constructor RequestBody
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
      #+ FC.required "fieldL" fieldL FieldL.fieldLSchema
      #+ FC.optional "fieldM" fieldM FieldM.fieldMSchema