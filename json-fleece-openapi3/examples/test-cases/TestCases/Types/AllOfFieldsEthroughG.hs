{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfFieldsEthroughG
  ( AllOfFieldsEthroughG(..)
  , allOfFieldsEthroughGSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Either, Eq, Maybe, Show)
import qualified TestCases.Types.AllOfFieldsEthroughG.FieldE as FieldE
import qualified TestCases.Types.AllOfFieldsEthroughG.FieldF as FieldF
import qualified TestCases.Types.SingletonNullableOneOfRef as SingletonNullableOneOfRef

data AllOfFieldsEthroughG = AllOfFieldsEthroughG
  { fieldE :: Maybe (Either FC.Null FieldE.FieldE)
  , fieldF :: Maybe FieldF.FieldF
  , fieldG :: Maybe (Either FC.Null SingletonNullableOneOfRef.SingletonNullableOneOfRef)
  }
  deriving (Eq, Show)

allOfFieldsEthroughGSchema :: FC.Fleece t => FC.Schema t AllOfFieldsEthroughG
allOfFieldsEthroughGSchema =
  FC.object $
    FC.constructor AllOfFieldsEthroughG
      #+ FC.optional "fieldE" fieldE (FC.nullable FieldE.fieldESchema)
      #+ FC.optional "fieldF" fieldF FieldF.fieldFSchema
      #+ FC.optional "fieldG" fieldG (FC.nullable SingletonNullableOneOfRef.singletonNullableOneOfRefSchema)