{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Fleece.OpenApi3.Schemas.Schemas
  ( FleeceOpenApi3
  , nonEmpty
  , set
  , nonEmptyText
  , integer
  , unboundedIntegralNumberNamed
  , unboundedIntegralNumber
  , boundedIntegralNumberNamed
  , boundedIntegralNumber
  , int
  , int8
  , int16
  , int32
  , int64
  , word
  , word8
  , word16
  , word32
  , word64
  , double
  , float
  , utcTime
  , utcTimeWithFormat
  , localTime
  , localTimeWithFormat
  , zonedTime
  , zonedTimeWithFormat
  , day
  , dayWithFormat
  , dateTimeFormat
  , customFormat
  ) where

import qualified Data.Int as I
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromJust)
import qualified Data.NonEmptyText as NET
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import qualified Data.Word as W
import qualified Fleece.Core as FC

import Fleece.OpenApi3.Schemas.OpenApi3Validator (OpenApi3Validator (maximumIntegral, minItems, minLength, minimumIntegral, setValidatorFormat, setValidatorType, uniqueItems))

type FleeceOpenApi3 schema =
  ( FC.Fleece schema
  , OpenApi3Validator (FC.Validator schema)
  )

nonEmpty :: FleeceOpenApi3 schema => schema a -> schema (NEL.NonEmpty a)
nonEmpty itemSchema =
  let
    validator =
      FC.transform (V.fromList . NEL.toList) (NEL.fromList . V.toList)
        `FC.compose` minItems 1
  in
    FC.validateNamed
      (FC.unqualifiedName $ "NonEmpty " <> FC.nameUnqualified (FC.schemaName itemSchema))
      validator
      (FC.array itemSchema)

set :: (Ord a, FleeceOpenApi3 schema) => FC.SetDuplicateHandling -> schema a -> schema (Set.Set a)
set handling itemSchema =
  FC.validateNamed
    (FC.unqualifiedName $ "Set [" <> FC.nameUnqualified (FC.schemaName itemSchema) <> "]")
    (uniqueItems handling)
    (FC.array itemSchema)

nonEmptyText :: FleeceOpenApi3 schema => schema NET.NonEmptyText
nonEmptyText =
  let
    validator =
      FC.transform NET.toText (fromJust . NET.fromText)
        `FC.compose` minLength 1
  in
    FC.validateNamed
      (FC.unqualifiedName "NonEmptyText")
      validator
      FC.text

integer :: FleeceOpenApi3 schema => schema Integer
integer = unboundedIntegralNumber

unboundedIntegralNumberNamed ::
  (FleeceOpenApi3 schema, Integral n) =>
  FC.Name ->
  schema n
unboundedIntegralNumberNamed name =
  FC.validateNamed
    name
    (setValidatorType "integer" FC.identity)
    (FC.unboundedIntegralNumberNamed name)

unboundedIntegralNumber ::
  (FleeceOpenApi3 schema, Integral n, Typeable n) =>
  schema n
unboundedIntegralNumber =
  let
    name =
      FC.defaultSchemaName schema

    schema =
      unboundedIntegralNumberNamed name
  in
    schema

boundedIntegralNumberNamed ::
  (FleeceOpenApi3 schema, Integral n, Bounded n) =>
  FC.Name ->
  schema n
boundedIntegralNumberNamed name =
  let
    validator =
      minimumIntegral minBound
        `FC.compose` maximumIntegral maxBound
  in
    FC.validateNamed
      name
      validator
      (unboundedIntegralNumberNamed name)

boundedIntegralNumber ::
  (FleeceOpenApi3 schema, Integral n, Bounded n, Typeable n) =>
  schema n
boundedIntegralNumber =
  let
    name =
      FC.defaultSchemaName schema

    schema =
      boundedIntegralNumberNamed name
  in
    schema

int :: FleeceOpenApi3 schema => schema Int
int = boundedIntegralNumber

int8 :: FleeceOpenApi3 schema => schema I.Int8
int8 = boundedIntegralNumber

int16 :: FleeceOpenApi3 schema => schema I.Int16
int16 = boundedIntegralNumber

int32 :: FleeceOpenApi3 schema => schema I.Int32
int32 = FC.validate (setValidatorFormat "int32" FC.identity) boundedIntegralNumber

int64 :: FleeceOpenApi3 schema => schema I.Int64
int64 = FC.validate (setValidatorFormat "int64" FC.identity) boundedIntegralNumber

word :: FleeceOpenApi3 schema => schema Word
word = boundedIntegralNumber

word8 :: FleeceOpenApi3 schema => schema W.Word8
word8 = boundedIntegralNumber

word16 :: FleeceOpenApi3 schema => schema W.Word16
word16 = boundedIntegralNumber

word32 :: FleeceOpenApi3 schema => schema W.Word32
word32 = boundedIntegralNumber

word64 :: FleeceOpenApi3 schema => schema W.Word64
word64 = boundedIntegralNumber

double :: FleeceOpenApi3 schema => schema Double
double = FC.validate (setValidatorFormat "double" FC.identity) FC.realFloat

float :: FleeceOpenApi3 schema => schema Float
float = FC.validate (setValidatorFormat "float" FC.identity) FC.realFloat

utcTime :: FleeceOpenApi3 schema => schema Time.UTCTime
utcTime = dateTimeFormat FC.utcTime

utcTimeWithFormat :: FleeceOpenApi3 schema => String -> schema Time.UTCTime
utcTimeWithFormat s = customFormat (T.pack s) $ FC.utcTimeWithFormat s

localTime :: FleeceOpenApi3 schema => schema Time.LocalTime
localTime = dateTimeFormat FC.localTime

localTimeWithFormat :: FleeceOpenApi3 schema => String -> schema Time.LocalTime
localTimeWithFormat s = customFormat (T.pack s) $ FC.localTimeWithFormat s

zonedTime :: FleeceOpenApi3 schema => schema Time.ZonedTime
zonedTime = dateTimeFormat FC.zonedTime

zonedTimeWithFormat :: FleeceOpenApi3 schema => String -> schema Time.ZonedTime
zonedTimeWithFormat s = customFormat (T.pack s) $ FC.zonedTimeWithFormat s

day :: FleeceOpenApi3 schema => schema Time.Day
day = FC.validateNamed "Day" (setValidatorFormat "date" FC.identity) FC.day

dayWithFormat :: FleeceOpenApi3 schema => String -> schema Time.Day
dayWithFormat s = customFormat (T.pack s) $ FC.dayWithFormat s

dateTimeFormat :: FleeceOpenApi3 schema => schema a -> schema a
dateTimeFormat schema =
  FC.validateNamed (FC.schemaName schema) (setValidatorFormat "date-time" FC.identity) schema

customFormat :: FleeceOpenApi3 schema => T.Text -> schema a -> schema a
customFormat format schema =
  FC.validateNamed (FC.schemaName schema) (setValidatorFormat format FC.identity) schema
