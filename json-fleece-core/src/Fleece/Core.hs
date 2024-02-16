{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Fleece.Core
  ( -- * A class for defining Fleece schemas
    Fleece
  , schemaName

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
  , transform
  , transformNamed
  , coerceSchema
  , coerceSchemaNamed

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
  , SetDuplicateHandling (AllowInputDuplicates, RejectInputDuplicates)
  , set
  , string
  , utcTime
  , localTime
  , zonedTime
  , day
  , dayWithFormat
  , boundedIntegralNumber
  , boundedIntegralNumberNamed
  , unboundedIntegralNumber
  , unboundedIntegralNumberNamed
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
  , bareOrJSONString

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
