{-# LANGUAGE TypeFamilies #-}

module Fleece.Aeson.Encoder
  ( Encoder
  , encode
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AesonEncoding
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import qualified Data.Vector as V
import qualified Fleece.Core as FC

data Encoder a
  = Encoder FC.Name (a -> Aeson.Encoding)

encode :: Encoder a -> a -> LBS.ByteString
encode (Encoder _name toEncoding) =
  AesonEncoding.encodingToLazyByteString . toEncoding

instance FC.Fleece Encoder where
  newtype Object Encoder object _constructor
    = Object (object -> Aeson.Series)

  newtype Field Encoder object _a
    = Field (object -> Aeson.Series)

  newtype EmbeddedObject Encoder object _a
    = EmbeddedObject (object -> Aeson.Series)

  schemaName (Encoder name _toEncoding) =
    name

  number =
    Encoder (FC.unqualifiedName "number") Aeson.toEncoding

  text =
    Encoder (FC.unqualifiedName "text") Aeson.toEncoding

  boolean =
    Encoder (FC.unqualifiedName "boolean") Aeson.toEncoding

  null =
    Encoder
      (FC.unqualifiedName "null")
      (\FC.Null -> Aeson.toEncoding Aeson.Null)

  array (Encoder name itemToEncoding) =
    Encoder
      (FC.annotateName name "array")
      (AesonTypes.listEncoding itemToEncoding . V.toList)

  nullable (Encoder name toEncoding) =
    Encoder (FC.annotateName name "nullable") $ \mbValue ->
      case mbValue of
        Left FC.Null -> Aeson.toEncoding Aeson.Null
        Right value -> toEncoding value

  required name accessor (Encoder _name toEncoding) =
    let
      key = AesonKey.fromString name
    in
      Field $ \object ->
        AesonEncoding.pair key (toEncoding (accessor object))

  optional name accessor (Encoder _name toEncoding) =
    let
      key = AesonKey.fromString name
    in
      Field $ \object ->
        case accessor object of
          Just value ->
            AesonEncoding.pair key (toEncoding value)
          Nothing ->
            mempty

  mapField _f encoder =
    coerce encoder

  constructor _f =
    Object (\_ -> mempty)

  field (Object mkStart) (Field mkNext) =
    Object $ \object ->
      mkStart object <> mkNext object

  embed (Object mkStart) (EmbeddedObject mkMore) =
    Object $ \object ->
      mkStart object <> mkMore object

  embedded accessor (Object mkFields) =
    EmbeddedObject (mkFields . accessor)

  objectNamed name (Object toSeries) =
    Encoder name (Aeson.pairs . toSeries)

  boundedEnumNamed name toText =
    Encoder name (Aeson.toEncoding . toText)

  validateNamed name uncheck _check (Encoder _unvalidatedName toEncoding) =
    Encoder name (toEncoding . uncheck)
