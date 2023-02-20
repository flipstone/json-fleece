{-# LANGUAGE TypeFamilies #-}

module Fleece.Core
  ( Fleece
      ( Field
      , Object
      , string
      , number
      , boolean
      , array
      , null
      , required
      , optionalField
      , objectNamed
      , constructor
      , nullable
      , field
      , validateNamed
      , boundedEnumNamed
      )
  , optional
  , object
  , boundedEnum
  , validate
  , list
  , transform
  , transformNamed
  , coerceSchema
  , coerceSchemaNamed
  , (#+)
  , Null (Null)
  , NullBehavior
    ( EmitNull_AcceptNull
    , OmitKey_AcceptNull
    , OmitKey_DelegateNull
    )
  ) where

import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Data.Typeable (Typeable, tyConName, typeRep, typeRepTyCon)
import qualified Data.Vector as V

data Null
  = Null
  deriving (Eq, Show)

class Fleece schema where
  data Object schema :: Type -> Type -> Type
  data Field schema :: Type -> Type -> Type

  number :: schema Scientific

  string :: schema T.Text

  boolean :: schema Bool

  array :: schema a -> schema (V.Vector a)

  null :: schema Null

  nullable :: schema a -> schema (Maybe a)

  required ::
    String ->
    (object -> a) ->
    schema a ->
    Field schema object a

  optionalField ::
    NullBehavior ->
    String ->
    (object -> Maybe a) ->
    schema a ->
    Field schema object (Maybe a)

  objectNamed ::
    String ->
    Object schema a a ->
    schema a

  constructor ::
    constructor ->
    Object schema object constructor

  field ::
    Object schema object (a -> b) ->
    Field schema object a ->
    Object schema object b

  validateNamed ::
    String ->
    (a -> b) ->
    (b -> Either String a) ->
    (schema b) ->
    (schema a)

  boundedEnumNamed ::
    (Bounded a, Enum a) =>
    String ->
    (a -> T.Text) ->
    schema a

(#+) ::
  Fleece schema =>
  Object schema object (a -> b) ->
  Field schema object a ->
  Object schema object b
(#+) =
  field

infixl 9 #+

data NullBehavior
  = EmitNull_AcceptNull
  | OmitKey_AcceptNull
  | OmitKey_DelegateNull

object ::
  (Fleece schema, Typeable a) =>
  Object schema a a ->
  schema a
object o =
  let
    name =
      defaultSchemaName schema

    schema =
      objectNamed name o
  in
    schema

boundedEnum ::
  (Fleece schema, Typeable a, Enum a, Bounded a) =>
  (a -> T.Text) ->
  schema a
boundedEnum toText =
  let
    name =
      defaultSchemaName schema

    schema =
      boundedEnumNamed name toText
  in
    schema

validate ::
  (Fleece schema, Typeable a) =>
  (a -> b) ->
  (b -> Either String a) ->
  schema b ->
  schema a
validate uncheck check schemaB =
  let
    name =
      defaultSchemaName schemaA

    schemaA =
      validateNamed name uncheck check schemaB
  in
    schemaA

transform ::
  (Fleece schema, Typeable a) =>
  (a -> b) ->
  (b -> a) ->
  schema b ->
  schema a
transform aToB bToA schemaB =
  let
    name =
      defaultSchemaName schemaA

    schemaA =
      transformNamed name aToB bToA schemaB
  in
    schemaA

transformNamed ::
  Fleece schema =>
  String ->
  (a -> b) ->
  (b -> a) ->
  schema b ->
  schema a
transformNamed name aToB bToA =
  validateNamed name aToB (Right . bToA)

coerceSchema ::
  (Fleece schema, Typeable a, Coercible a b) =>
  schema b ->
  schema a
coerceSchema schemaB =
  let
    name =
      defaultSchemaName schemaA

    schemaA =
      coerceSchemaNamed name schemaB
  in
    schemaA

coerceSchemaNamed ::
  (Fleece schema, Coercible a b) =>
  String ->
  schema b ->
  schema a
coerceSchemaNamed name schemaB =
  transformNamed name coerce coerce schemaB

optional ::
  Fleece schema =>
  String ->
  (object -> Maybe a) ->
  schema a ->
  Field schema object (Maybe a)
optional = optionalField OmitKey_AcceptNull

list :: Fleece schema => schema a -> schema [a]
list itemSchema =
  transformNamed
    "List"
    V.fromList
    V.toList
    (array itemSchema)

-- Internal Helpers

defaultSchemaName ::
  Typeable a =>
  schema a ->
  String
defaultSchemaName =
  tyConName
    . typeRepTyCon
    . typeRep
