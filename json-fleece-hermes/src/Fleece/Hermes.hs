{-# LANGUAGE TypeFamilies #-}

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

  {-# INLINE inlineObject #-}
  inlineObject (Object fieldNames parseF) (FC.InlineObject _accessor inlineSchema) =
    Object
      { objectFields = objectFields inlineSchema <> fieldNames
      , objectDecoder = parseF <*> objectDecoder inlineSchema
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

  {-# INLINE resolveObjectValidation #-}
  resolveObjectValidation (Object fieldNames parseErrOrObject) =
    Object
      { objectFields = fieldNames
      , objectDecoder = do
          errOrObject <- parseErrOrObject
          case errOrObject of
            Left err -> fail err
            Right object -> pure object
      }

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

  jsonString (Decoder name parseValue) =
    Decoder name $
      H.withText $ \jsonText -> do
        -- hermes currently cannot decode scalars. So we make a dummy object.
        let
          obj = Enc.encodeUtf8 $ T.pack "{ \"\": " <> jsonText <> T.pack "}"
        case H.decodeEither (H.object $ H.atKey (T.pack "") parseValue) obj of
          Left err -> fail ("Error decoding nested json string: " <> show err)
          Right value -> pure value
