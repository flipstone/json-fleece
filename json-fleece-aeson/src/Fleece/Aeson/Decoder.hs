{-# LANGUAGE TypeFamilies #-}

module Fleece.Aeson.Decoder
  ( Decoder
  , decode
  , fromValue
  ) where

import Control.Monad ((<=<))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LBS
import qualified Fleece.Core as FC

newtype Decoder a
  = Decoder (Aeson.Value -> AesonTypes.Parser a)

fromValue :: Decoder a -> Aeson.Value -> Either String a
fromValue (Decoder f) = AesonTypes.parseEither f

decode :: Decoder a -> LBS.ByteString -> Either String a
decode decoder =
  fromValue decoder <=< Aeson.eitherDecode

instance FC.Fleece Decoder where
  data Object Decoder _object a
    = Object (Aeson.Object -> AesonTypes.Parser a)

  data Field Decoder _object a
    = Field (Aeson.Object -> AesonTypes.Parser a)

  number =
    Decoder $ Aeson.withScientific "number" pure

  text =
    Decoder $ Aeson.withText "text" pure

  nullable (Decoder parseValue) =
    Decoder $ \value ->
      case value of
        Aeson.Null -> pure Nothing
        _ -> fmap Just (parseValue value)

  required name _accessor (Decoder parseValue) =
    Field $ \object ->
      AesonTypes.explicitParseField
        parseValue
        object
        (AesonKey.fromString name)

  optionalField nullBehavior name _accessor (Decoder parseValue) =
    let
      key = AesonKey.fromString name
    in
      Field $ \object ->
        case nullBehavior of
          FC.EmitNull_AcceptNull ->
            AesonTypes.explicitParseFieldMaybe parseValue object key
          FC.OmitKey_AcceptNull ->
            AesonTypes.explicitParseFieldMaybe parseValue object key
          FC.OmitKey_DelegateNull ->
            AesonTypes.explicitParseFieldMaybe' parseValue object key

  constructor f =
    Object (\_object -> pure f)

  field (Object parseF) (Field parseField) =
    Object (\object -> parseF object <*> parseField object)

  object (Object f) =
    Decoder $ Aeson.withObject "object" $ \object ->
      f object
