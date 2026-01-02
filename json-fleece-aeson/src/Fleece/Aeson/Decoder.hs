{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Fleece.Aeson.Decoder
  ( Decoder (Decoder)
  , toParser
  , decode
  , decodeStrict
  , fromLazyText
  , fromStrictText
  , fromValue
  ) where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy as TL
import GHC.TypeLits (KnownNat, KnownSymbol, symbolVal)
import Shrubbery (type (@=))
import qualified Shrubbery

import qualified Fleece.Core as FC

newtype Decoder a
  = Decoder (Aeson.Value -> AesonTypes.Parser a)

toParser :: FC.Schema Decoder a -> Aeson.Value -> AesonTypes.Parser a
toParser (FC.Schema _ (Decoder f)) = f

fromLazyText :: FC.Schema Decoder a -> TL.Text -> Either String a
fromLazyText decoder =
  fromStrictText decoder . TL.toStrict

fromStrictText :: FC.Schema Decoder a -> T.Text -> Either String a
fromStrictText decoder =
  -- With aeson-2.2.1.0, there is no need to encode to UTF-8,
  -- this could be 'fromValue decoder <=< eitherDecodeStrictText'
  decodeStrict decoder . Enc.encodeUtf8

fromValue :: FC.Schema Decoder a -> Aeson.Value -> Either String a
fromValue =
  AesonTypes.parseEither . toParser

decode :: FC.Schema Decoder a -> LBS.ByteString -> Either String a
decode decoder =
  fromValue decoder <=< Aeson.eitherDecode

decodeStrict :: FC.Schema Decoder a -> BS.ByteString -> Either String a
decodeStrict decoder =
  fromValue decoder <=< Aeson.eitherDecodeStrict

instance FC.Fleece Decoder where
  data Object Decoder _object a = Object
    { objectFields :: [AesonKey.Key]
    , objectDecoder :: Aeson.Object -> AesonTypes.Parser a
    }

  data Field Decoder _object a = Field
    { fieldName :: AesonKey.Key
    , fieldDecoder :: Aeson.Object -> AesonTypes.Parser a
    }

  newtype AdditionalFields Decoder _object a = AdditionalFields
    { additionalFieldsDecoder :: [AesonKey.Key] -> Aeson.Object -> AesonTypes.Parser a
    }

  newtype UnionMembers Decoder allTypes _handledTypes
    = UnionMembers (FC.Name -> Aeson.Value -> AesonTypes.Parser (Shrubbery.Union allTypes))

  newtype TaggedUnionMembers Decoder allTags _handledTags
    = TaggedUnionMembers (Map.Map T.Text (Aeson.Object -> AesonTypes.Parser (Shrubbery.TaggedUnion allTags)))

  interpretFormat _ =
    FC.schemaInterpreter

  interpretNumber name =
    Decoder $ Aeson.withScientific (FC.nameToString name) pure

  interpretText name =
    Decoder $ Aeson.withText (FC.nameToString name) pure

  interpretBoolean name =
    Decoder $ Aeson.withBool (FC.nameToString name) pure

  interpretArray arrayName (FC.Schema _itemSchemaName (Decoder itemFromValue)) =
    Decoder $ Aeson.withArray (FC.nameToString arrayName) (traverse itemFromValue)

  interpretNull name =
    Decoder $ \value ->
      case value of
        Aeson.Null -> pure FC.Null
        _ -> AesonTypes.typeMismatch (FC.nameToString name) value

  interpretNullable _nullableName (FC.Schema _schemaName (Decoder parseValue)) =
    Decoder $ \value ->
      case value of
        Aeson.Null -> pure (Left FC.Null)
        _ -> fmap Right (parseValue value)

  required name _accessor (FC.Schema _name (Decoder parseValue)) =
    let
      key = AesonKey.fromString name
    in
      Field key $ \object ->
        AesonTypes.explicitParseField parseValue object key

  optional name _accessor (FC.Schema _name (Decoder parseValue)) =
    let
      key = AesonKey.fromString name
    in
      Field key $ \object ->
        AesonTypes.explicitParseFieldMaybe' parseValue object key

  additionalFields _accessor (FC.Schema _name (Decoder parseValue)) =
    AdditionalFields $ \definedFields object ->
      let
        additionalKeysMap =
          foldr
            Map.delete
            (AesonKeyMap.toMap object)
            definedFields
      in
        fmap (Map.mapKeys AesonKey.toText)
          . traverse parseValue
          $ additionalKeysMap

  mapField f (Field name parseField) =
    Field name (fmap f . parseField)

  constructor f =
    Object [] (\_object -> pure f)

  field (Object fieldNames parseF) field =
    Object
      { objectFields = fieldName field : fieldNames
      , objectDecoder =
          \object ->
            parseF object
              <*> fieldDecoder field object
      }

  additional (Object fieldNames parseF) fields =
    Object
      { objectFields = fieldNames
      , objectDecoder =
          \object ->
            parseF object
              <*> additionalFieldsDecoder fields fieldNames object
      }

  interpretObjectNamed name (Object _definedFields parseObject) =
    Decoder $
      Aeson.withObject (FC.nameToString name) parseObject

  interpretBoundedEnumNamed name toText =
    let
      decodingMap =
        Map.fromList
          . map (\e -> (toText e, e))
          $ [minBound .. maxBound]
    in
      Decoder $
        Aeson.withText (FC.nameToString name) $ \textValue ->
          case Map.lookup textValue decodingMap of
            Just enumValue -> pure enumValue
            Nothing ->
              fail $
                "Unrecognized value for "
                  <> FC.nameToString name
                  <> " enum: "
                  <> show textValue

  interpretValidateNamed name _uncheck check (FC.Schema _unvalidatedName (Decoder parseValue)) =
    validatingDecoder name parseValue check

  interpretValidateAnonymous _uncheck check (FC.Schema unvalidatedName (Decoder parseValue)) =
    validatingDecoder unvalidatedName parseValue check

  interpretUnionNamed name (UnionMembers parseMembers) =
    Decoder (parseMembers name)

  unionMemberWithIndex index (FC.Schema _name (Decoder parseMember)) =
    UnionMembers (\_name -> fmap (Shrubbery.unifyUnion index) . parseMember)

  unionCombine (UnionMembers parseLeft) (UnionMembers parseRight) =
    UnionMembers $ \name value ->
      parseLeft name value
        <|> parseRight name value
        <|> fail ("All union parsing options for " <> FC.nameUnqualified name <> " failed.")

  interpretTaggedUnionNamed name tagProperty (TaggedUnionMembers parserMap) =
    let
      tagPropKey =
        AesonKey.fromString tagProperty

      nameString =
        FC.nameToString name

      parseTag =
        Aeson.withText (tagProperty <> " field of " <> nameString) pure
    in
      Decoder $
        Aeson.withObject nameString $ \object -> do
          tagValue <- AesonTypes.explicitParseField parseTag object tagPropKey

          case Map.lookup tagValue parserMap of
            Just parseObject -> parseObject object
            Nothing -> fail ("Invalid tag found for tagged union " <> nameString <> ": " <> T.unpack tagValue)

  taggedUnionMemberWithTag ::
    forall tag allTags a proxy n.
    ( KnownSymbol tag
    , n ~ Shrubbery.TagIndex tag allTags
    , KnownNat n
    , Shrubbery.TagType tag allTags ~ a
    , Shrubbery.TypeAtIndex n (Shrubbery.TaggedTypes allTags) ~ a
    ) =>
    proxy tag ->
    FC.Object Decoder a a ->
    FC.TaggedUnionMembers Decoder allTags '[tag @= a]
  taggedUnionMemberWithTag tag object =
    let
      tagValue =
        T.pack (symbolVal tag)

      parseUnion =
        fmap (Shrubbery.unifyTaggedUnion @tag) . objectDecoder object
    in
      TaggedUnionMembers (Map.singleton tagValue parseUnion)

  taggedUnionCombine (TaggedUnionMembers left) (TaggedUnionMembers right) =
    TaggedUnionMembers (Map.union left right)

  interpretJsonString (FC.Schema name (Decoder parseValue)) =
    Decoder $
      Aeson.withText (FC.nameUnqualified name) $ \jsonText ->
        case Aeson.eitherDecodeStrict (Enc.encodeUtf8 jsonText) of
          Left err -> fail ("Error decoding nested json string:" <> err)
          Right value -> parseValue value

validatingDecoder ::
  FC.Name ->
  (Aeson.Value -> AesonTypes.Parser a) ->
  (a -> Either String b) ->
  Decoder b
validatingDecoder name parseValue check =
  Decoder $ \jsonValue -> do
    uncheckedValue <- parseValue jsonValue
    case check uncheckedValue of
      Right checkedValue -> pure checkedValue
      Left err -> fail $ "Error validating " <> FC.nameToString name <> ": " <> err
