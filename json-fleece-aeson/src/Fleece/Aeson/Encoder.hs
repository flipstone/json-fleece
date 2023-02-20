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
import qualified Data.Vector as V
import qualified Fleece.Core as FC

newtype Encoder a
  = Encoder (a -> Aeson.Encoding)

encode :: Encoder a -> a -> LBS.ByteString
encode (Encoder toEncoding) =
  AesonEncoding.encodingToLazyByteString . toEncoding

instance FC.Fleece Encoder where
  data Object Encoder object _constructor
    = Object (object -> Aeson.Series)

  data Field Encoder object _a
    = Field (object -> Aeson.Series)

  number =
    Encoder Aeson.toEncoding

  string =
    Encoder Aeson.toEncoding

  boolean =
    Encoder Aeson.toEncoding

  null =
    Encoder (\FC.Null -> Aeson.toEncoding Aeson.Null)

  array (Encoder itemToEncoding) =
    Encoder (AesonTypes.listEncoding itemToEncoding . V.toList)

  nullable (Encoder toEncoding) =
    Encoder $ \mbValue ->
      case mbValue of
        Nothing -> Aeson.toEncoding Aeson.Null
        Just value -> toEncoding value

  required name accessor (Encoder toEncoding) =
    Field $ \object ->
      AesonEncoding.pair
        (AesonKey.fromString name)
        (toEncoding (accessor object))

  optionalField nullBehavior name accessor (Encoder toEncoding) =
    let
      key = AesonKey.fromString name
    in
      Field $ \object ->
        case (accessor object, nullBehavior) of
          (Just value, _) ->
            AesonEncoding.pair key (toEncoding value)
          (Nothing, FC.EmitNull_AcceptNull) ->
            AesonEncoding.pair key (Aeson.toEncoding Aeson.Null)
          (Nothing, FC.OmitKey_AcceptNull) ->
            mempty
          (Nothing, FC.OmitKey_DelegateNull) ->
            mempty

  constructor _f =
    Object (\_ -> mempty)

  field (Object mkStart) (Field mkNext) =
    Object $ \object ->
      mkStart object <> mkNext object

  objectNamed _name (Object toSeries) =
    Encoder (Aeson.pairs . toSeries)

  boundedEnumNamed _name toText =
    Encoder (Aeson.toEncoding . toText)

  validateNamed _name uncheck _check (Encoder toEncoding) =
    Encoder (toEncoding . uncheck)
