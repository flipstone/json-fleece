{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Fleece.Core
  ( -- * A class for defining Fleece schemas
    Fleece
  , Schema (Schema, schemaName, schemaInterpreter, schemaDescription)
  , hoistSchema
  , describeSchema
  , format

    -- * 'Fleece' class methods used to implement primitives and combinators
  , interpretFormat
  , interpretText
  , interpretNumber
  , interpretBoolean
  , interpretArray
  , interpretNull
  , interpretObjectNamed
  , interpretNullable
  , interpretValidateNamed
  , interpretValidateAnonymous
  , interpretBoundedEnumNamed
  , interpretUnionNamed
  , interpretTaggedUnionNamed
  , interpretJsonString
  , interpretInt
  , interpretInt8
  , interpretInt16
  , interpretInt32
  , interpretInt64
  , interpretWord
  , interpretWord8
  , interpretWord16
  , interpretWord32
  , interpretWord64
  , interpretDouble
  , interpretFloat

    -- * Schemas for dealing with JSON primitives
  , text
  , number
  , boolean
  , array
  , Null (Null)
  , Fleece.Core.Class.null

    -- * Combinators for building JSON objects from primitives
  , Object
  , object
  , objectNamed
  , constructor
  , field
  , (#+)
  , Field
  , required
  , optional
  , optionalNullable
  , NothingEncoding (EmitNull, OmitKey)
  , mapField
  , additional
  , (#*)
  , AdditionalFields
  , additionalFields

    -- * Combinators for changing the Haskell data type that a schema serializes

  -- to/from
  , nullable
  , validate
  , validateNamed
  , validateAnonymous
  , transform
  , transformNamed
  , transformAnonymous
  , coerceSchema
  , coerceSchemaNamed
  , coerceSchemaAnonymous

    -- * Schemas for common Haskell data types that are useful with JSON
  , boundedEnum
  , boundedEnumNamed
  , list
  , Fleece.Core.Schemas.map
  , nonEmpty
  , nonEmptyText
  , integer
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
  , realFloat
  , realFloatNamed
  , realFloatAnonymous
  , fixed
  , SetDuplicateHandling (AllowInputDuplicates, RejectInputDuplicates)
  , set
  , string
  , utcTime
  , utcTimeWithFormat
  , localTime
  , localTimeWithFormat
  , zonedTime
  , zonedTimeWithFormat
  , day
  , dayWithFormat
  , timeWithFormat
  , boundedIntegralNumber
  , boundedIntegralNumberNamed
  , boundedIntegralNumberAnonymous
  , unboundedIntegralNumber
  , unboundedIntegralNumberNamed
  , unboundedIntegralNumberAnonymous
  , eitherOf
  , eitherOfNamed

    -- * Functions for building schemas for anonymous unions
  , union
  , unionNamed
  , UnionMembers
  , unionMember
  , unionMemberWithIndex
  , unionCombine
  , (#|)

    -- * Functions for building schemas for tagged unions

  -- , union
  , taggedUnionNamed
  , TaggedUnionMembers
  , taggedUnionMember
  , taggedUnionMemberWithTag
  , taggedUnionCombine
  , (#@)

    -- * A type and schema for handling arbitrary JSON
  , AnyJSON (AnyJSON)
  , anyJSON
  , mkJSONText
  , getJSONText
  , mkJSONBool
  , getJSONBool
  , mkJSONNumber
  , getJSONNumber
  , mkJSONArray
  , getJSONArray
  , mkJSONObject
  , getJSONObject
  , mkJSONNull
  , getJSONNull
  , handleAnyJSON

    -- * Schema helpers for dealing with JSON strings within json
  , jsonString

    -- * Schema Names
  , Name (nameQualification, nameUnqualified)
  , unqualifiedName
  , qualifiedName
  , autoQualifiedName
  , nameToString
  , annotateName
  ) where

import Fleece.Core.AnyJSON
import Fleece.Core.Class
import Fleece.Core.Name
import Fleece.Core.Schemas
