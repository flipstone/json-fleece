{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Fleece.Hermes
  ( Decoder
  , decode
  , toDecoder
  ) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.Hermes as H
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import GHC.TypeLits (KnownNat, KnownSymbol, symbolVal)
import Shrubbery (type (@=))
import qualified Shrubbery

import qualified Fleece.Core as FC

data Decoder a
  = Decoder FC.Name (H.Decoder a)

toDecoder :: Decoder a -> H.Decoder a
toDecoder (Decoder _name f) = f

decode :: Decoder a -> BS.ByteString -> Either String a
decode decoder input =
  first (T.unpack . H.formatException) $
    H.decodeEither (toDecoder decoder) input

instance FC.Fleece Decoder where
  data Object Decoder _object a = Object
    { objectFields :: [Text]
    , objectDecoder :: H.FieldsDecoder a
    }

  data Field Decoder _object a = Field
    { fieldName :: Text
    , fieldDecoder :: H.FieldsDecoder a
    }

  newtype AdditionalFields Decoder _object a = AdditionalFields
    { additionalFieldsDecoder :: [Text] -> H.FieldsDecoder a
    }

  newtype UnionMembers Decoder allTypes _handledTypes
    = UnionMembers (FC.Name -> H.Decoder (Shrubbery.Union allTypes))

  newtype TaggedUnionMembers Decoder allTags _handledTags
    = TaggedUnionMembers (Map.Map T.Text (H.FieldsDecoder (Shrubbery.TaggedUnion allTags)))

  schemaName (Decoder name _parseValue) =
    name

  {-# INLINE number #-}
  number =
    Decoder (FC.unqualifiedName "number") H.scientific

  {-# INLINE text #-}
  text =
    Decoder (FC.unqualifiedName "text") H.text

  {-# INLINE boolean #-}
  boolean =
    Decoder (FC.unqualifiedName "boolean") H.bool

  {-# INLINE array #-}
  array (Decoder name itemFromValue) =
    Decoder (FC.annotateName name "array") $
      H.vector itemFromValue

  {-# INLINE null #-}
  null =
    Decoder (FC.unqualifiedName "null") $ do
      isNull <- H.isNull
      if isNull
        then pure FC.Null
        else fail "expected null"

  {-# INLINE nullable #-}
  nullable (Decoder name parseValue) =
    Decoder (FC.annotateName name "nullable") $ do
      isNull <- H.isNull
      if isNull
        then pure (Left FC.Null)
        else fmap Right parseValue

  {-# INLINE required #-}
  required name _accessor (Decoder _name parseValue) =
    let
      key = T.pack name
    in
      Field key (H.atKey key parseValue)

  {-# INLINE optional #-}
  optional name _accessor (Decoder _name parseValue) =
    let
      key = T.pack name
    in
      Field key (H.atKeyOptional key parseValue)

  {-# INLINE additionalFields #-}
  additionalFields _accessor (Decoder _name parseValue) =
    AdditionalFields $ \definedFields ->
      H.liftObjectDecoder $ H.objectAsMapExcluding definedFields pure parseValue

  {-# INLINE mapField #-}
  mapField f (Field name parseField) =
    Field name (fmap f parseField)

  {-# INLINE constructor #-}
  constructor f =
    Object [] (pure f)

  {-# INLINE field #-}
  field (Object fieldNames parseF) field =
    Object
      { objectFields = fieldName field : fieldNames
      , objectDecoder =
          parseF <*> fieldDecoder field
      }

  {-# INLINE additional #-}
  additional (Object fieldNames parseF) fields =
    Object
      { objectFields = fieldNames
      , objectDecoder =
          parseF <*> additionalFieldsDecoder fields fieldNames
      }

  {-# INLINE objectNamed #-}
  objectNamed name (Object _definedFields parseObject) =
    Decoder name $ H.object parseObject

  {-# INLINE boundedEnumNamed #-}
  boundedEnumNamed name toText =
    let
      decodingMap =
        Map.fromList
          . map (\e -> (toText e, e))
          $ [minBound .. maxBound]
    in
      Decoder name $
        H.withText $ \textValue ->
          case Map.lookup textValue decodingMap of
            Just enumValue -> pure enumValue
            Nothing ->
              fail $
                "Unrecognized value for "
                  <> FC.nameToString name
                  <> " enum: "
                  <> show textValue

  validateNamed name _uncheck check (Decoder _unvalidatedName parseValue) =
    Decoder name $ do
      uncheckedValue <- parseValue
      case check uncheckedValue of
        Right checkedValue -> pure checkedValue
        Left err -> fail $ "Error validating " <> FC.nameToString name <> ": " <> err

  {-# INLINE unionNamed #-}
  unionNamed name (UnionMembers parseMembers) =
    Decoder name (parseMembers name)

  {-# INLINE unionMemberWithIndex #-}
  unionMemberWithIndex index (Decoder _name parseMember) =
    UnionMembers (\_name -> fmap (Shrubbery.unifyUnion index) parseMember)

  {-# INLINE unionCombine #-}
  unionCombine (UnionMembers parseLeft) (UnionMembers parseRight) =
    UnionMembers $ \name ->
      parseLeft name
        <|> parseRight name
        <|> fail ("All union parsing options for " <> FC.nameUnqualified name <> " failed.")

  taggedUnionNamed name tagProperty (TaggedUnionMembers parserMap) =
    let
      tagPropKey =
        T.pack tagProperty

      nameString =
        FC.nameToString name
    in
      Decoder name $
        H.object $ do
          tagValue <- H.atKey tagPropKey H.text
          pure ()
          case Map.lookup tagValue parserMap of
            Just parseFields -> parseFields
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
        fmap (Shrubbery.unifyTaggedUnion @tag)
          . objectDecoder
          $ object
    in
      TaggedUnionMembers (Map.singleton tagValue parseUnion)

  {-# INLINE taggedUnionCombine #-}
  taggedUnionCombine (TaggedUnionMembers left) (TaggedUnionMembers right) =
    TaggedUnionMembers (Map.union left right)

  jsonString (Decoder name parseValue) =
    Decoder name $
      H.withText $ \jsonText -> do
        -- hermes currently cannot decode scalars. So we make a dummy object.
        let
          obj = Enc.encodeUtf8 $ T.pack "{ \"\": " <> jsonText <> T.pack "}"
        case H.decodeEither (H.object $ H.atKey (T.pack "") parseValue) obj of
          Left err -> fail ("Error decoding nested json string: " <> show err)
          Right value -> pure value
