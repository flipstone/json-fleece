{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Fleece.Aeson.Decoder
  ( Decoder (..)
  , decode
  , decodeStrict
  , fromLazyText
  , fromStrictText
  , fromValue
  , toParser
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

data Decoder a
  = Decoder FC.Name (Aeson.Value -> AesonTypes.Parser a)

fromLazyText :: Decoder a -> TL.Text -> Either String a
fromLazyText decoder =
  fromStrictText decoder . TL.toStrict

fromStrictText :: Decoder a -> T.Text -> Either String a
fromStrictText decoder =
  -- With aeson-2.2.1.0, there is no need to encode to UTF-8,
  -- this could be 'fromValue decoder <=< eitherDecodeStrictText'
  decodeStrict decoder . Enc.encodeUtf8

fromValue :: Decoder a -> Aeson.Value -> Either String a
fromValue =
  AesonTypes.parseEither . toParser

toParser :: Decoder a -> Aeson.Value -> AesonTypes.Parser a
toParser (Decoder _name f) =
  f

decode :: Decoder a -> LBS.ByteString -> Either String a
decode decoder =
  fromValue decoder <=< Aeson.eitherDecode

decodeStrict :: Decoder a -> BS.ByteString -> Either String a
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
      Field key $ \object ->
        AesonTypes.explicitParseField parseValue object key

  optional name _accessor (Decoder _name parseValue) =
    let
      key = AesonKey.fromString name
    in
      Field key $ \object ->
        AesonTypes.explicitParseFieldMaybe' parseValue object key

  additionalFields _accessor (Decoder _name parseValue) =
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

  objectNamed name (Object _definedFields parseObject) =
    Decoder name $
      Aeson.withObject (FC.nameToString name) parseObject

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

  unionNamed name (UnionMembers parseMembers) =
    Decoder name (parseMembers name)

  unionMemberWithIndex index (Decoder _name parseMember) =
    UnionMembers (\_name -> fmap (Shrubbery.unifyUnion index) . parseMember)

  unionCombine (UnionMembers parseLeft) (UnionMembers parseRight) =
    UnionMembers $ \name value ->
      parseLeft name value
        <|> parseRight name value
        <|> fail ("All union parsing options for " <> FC.nameUnqualified name <> " failed.")

  taggedUnionNamed name tagProperty (TaggedUnionMembers parserMap) =
    let
      tagPropKey =
        AesonKey.fromString tagProperty

      nameString =
        FC.nameToString name

      parseTag =
        Aeson.withText (tagProperty <> " field of " <> nameString) pure
    in
      Decoder name $
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

  jsonString (Decoder name parseValue) =
    Decoder name $
      Aeson.withText (FC.nameUnqualified name) $ \jsonText ->
        case Aeson.eitherDecodeStrict (Enc.encodeUtf8 jsonText) of
          Left err -> fail ("Error decoding nested json string:" <> err)
          Right value -> parseValue value
