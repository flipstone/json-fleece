{-# LANGUAGE TypeFamilies #-}

module Fleece.Core
  ( Fleece (Field, Object, number, required, optionalField, text, object, constructor, nullable, field)
  , optional
  , (#+)
  , NullBehavior (..)
  ) where

import Data.Kind (Type)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Data.Typeable (Typeable)

class Fleece schema where
  data Object schema :: Type -> Type -> Type
  data Field schema :: Type -> Type -> Type

  number :: schema Scientific

  text :: schema T.Text

  nullable :: schema a -> schema (Maybe a)

  required ::
    Typeable a =>
    String ->
    (object -> a) ->
    schema a ->
    Field schema object a

  optionalField ::
    Typeable a =>
    NullBehavior ->
    String ->
    (object -> Maybe a) ->
    schema a ->
    Field schema object (Maybe a)

  object ::
    Typeable a =>
    Object schema a a ->
    schema a

  constructor ::
    constructor ->
    Object schema object constructor

  field ::
    Typeable a =>
    Object schema object (a -> b) ->
    Field schema object a ->
    Object schema object b

(#+) ::
  (Fleece schema, Typeable a) =>
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

optional ::
  (Fleece schema, Typeable a) =>
  String ->
  (object -> Maybe a) ->
  schema a ->
  Field schema object (Maybe a)
optional = optionalField OmitKey_AcceptNull
