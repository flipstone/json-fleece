{-# LANGUAGE TypeFamilies #-}

module Fleece.Core
  ( Fleece (Field, Object, number, required, optionalField, text, objectNamed, constructor, nullable, field)
  , object
  , optional
  , (#+)
  , NullBehavior (..)
  ) where

import Data.Kind (Type)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Data.Typeable (Typeable, tyConName, typeRep, typeRepTyCon)

class Fleece schema where
  data Object schema :: Type -> Type -> Type
  data Field schema :: Type -> Type -> Type

  number :: schema Scientific

  text :: schema T.Text

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
      tyConName
        . typeRepTyCon
        . typeRep
        $ schema

    schema =
      objectNamed name o
  in
    schema

optional ::
  Fleece schema =>
  String ->
  (object -> Maybe a) ->
  schema a ->
  Field schema object (Maybe a)
optional = optionalField OmitKey_AcceptNull
