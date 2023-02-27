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
import qualified Data.Map.Strict as Map
import qualified Fleece.Core as FC

data Decoder a
  = Decoder FC.Name (Aeson.Value -> AesonTypes.Parser a)

fromValue :: Decoder a -> Aeson.Value -> Either String a
fromValue (Decoder _name f) = AesonTypes.parseEither f

decode :: Decoder a -> LBS.ByteString -> Either String a
decode decoder =
  fromValue decoder <=< Aeson.eitherDecode

instance FC.Fleece Decoder where
  newtype Object Decoder _object a
    = Object (Aeson.Object -> AesonTypes.Parser a)

  newtype Field Decoder _object a
    = Field (Aeson.Object -> AesonTypes.Parser a)

  newtype EmbeddedObject Decoder _object a
    = EmbeddedObject (Aeson.Object -> AesonTypes.Parser a)

  schemaName (Decoder name _parseValue) =
    name

  number =
    Decoder (FC.unqualifiedName "number") $ Aeson.withScientific "number" pure

  text =
    Decoder (FC.unqualifiedName "text") $ Aeson.withText "text" pure

  boolean =
    Decoder (FC.unqualifiedName "boolean") $ Aeson.withBool "boolean" pure

  array (Decoder name itemFromValue) =
    Decoder (FC.annotateName name "array") $
      Aeson.withArray "array" (traverse itemFromValue)

  null =
    Decoder (FC.unqualifiedName "null") $ \value ->
      case value of
        Aeson.Null -> pure FC.Null
        _ -> AesonTypes.typeMismatch "Null" value

  nullable (Decoder name parseValue) =
    Decoder (FC.annotateName name "nullable") $ \value ->
      case value of
        Aeson.Null -> pure (Left FC.Null)
        _ -> fmap Right (parseValue value)

  required name _accessor (Decoder _name parseValue) =
    let
      key = AesonKey.fromString name
    in
      Field $ \object ->
        AesonTypes.explicitParseField parseValue object key

  optional name _accessor (Decoder _name parseValue) =
    let
      key = AesonKey.fromString name
    in
      Field $ \object ->
        AesonTypes.explicitParseFieldMaybe' parseValue object key

  mapField f (Field parseField) =
    Field (fmap f . parseField)

  constructor f =
    Object (\_object -> pure f)

  field (Object parseF) (Field parseField) =
    Object (\object -> parseF object <*> parseField object)

  embed (Object parseF) (EmbeddedObject parseSubobject) =
    Object (\object -> parseF object <*> parseSubobject object)

  embedded _accessor (Object parseObject) =
    EmbeddedObject parseObject

  objectNamed name (Object f) =
    Decoder name $
      Aeson.withObject (FC.nameToString name) $ \object ->
        f object

  boundedEnumNamed name toText =
    let
      decodingMap =
        Map.fromList
          . map (\e -> (toText e, e))
          $ [minBound .. maxBound]
    in
      Decoder name $
        Aeson.withText (FC.nameToString name) $ \textValue ->
          case Map.lookup textValue decodingMap of
            Just enumValue -> pure enumValue
            Nothing ->
              fail $
                "Unrecognized value for "
                  <> FC.nameToString name
                  <> " enum: "
                  <> show textValue

  validateNamed name _uncheck check (Decoder _unvalidatedName parseValue) =
    Decoder name $ \jsonValue -> do
      uncheckedValue <- parseValue jsonValue
      case check uncheckedValue of
        Right checkedValue -> pure checkedValue
        Left err -> fail $ "Error validating " <> FC.nameToString name <> ": " <> err
