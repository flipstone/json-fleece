{-# LANGUAGE TypeFamilies #-}
module Fleece.Core.SchemaName
  ( SchemaName
  , schemaName
  ) where

import Fleece.Core.Class
  ( AdditionalFields
  , Field
  , Fleece
  , Object
  , TaggedUnionMembers
  , UnionMembers
  , additional
  , additionalFields
  , array
  , boolean
  , boundedEnumNamed
  , constructor
  , field
  , jsonString
  , mapField
  , null
  , nullable
  , number
  , objectNamed
  , optional
  , required
  , taggedUnionCombine
  , taggedUnionMemberWithTag
  , taggedUnionNamed
  , text
  , unionCombine
  , unionMemberWithIndex
  , unionNamed
  , validateNamed
  )
import Fleece.Core.Name (Name, annotateName, unqualifiedName)

newtype SchemaName a = SchemaName {schemaName :: Name}

instance Fleece SchemaName where
  newtype Object SchemaName _a _b = Object ()
  newtype Field SchemaName _a _b = Field ()
  newtype AdditionalFields SchemaName _a _b = AdditionalFields ()
  newtype UnionMembers SchemaName _a _b = UnionMembers ()
  newtype TaggedUnionMembers SchemaName _a _b = TaggedUnionMembers ()

  number = SchemaName (unqualifiedName "number")

  text = SchemaName (unqualifiedName "text")

  boolean = SchemaName (unqualifiedName "boolean")

  array (SchemaName name) = SchemaName (annotateName name "array")

  null = SchemaName (unqualifiedName "null")

  nullable (SchemaName name) = SchemaName (annotateName name "nullable")

  required _ _ _ = Field ()

  optional _ _ _ = Field ()

  mapField _ _ = Field ()

  additionalFields _ _ = AdditionalFields ()

  objectNamed name _ = SchemaName name

  constructor _ = Object ()

  field _ _ = Object ()

  additional _ _ = Object ()

  validateNamed name _ _ _ = SchemaName name

  boundedEnumNamed name _ = SchemaName name

  unionNamed name _ = SchemaName name

  unionMemberWithIndex _ _ = UnionMembers ()

  unionCombine _ _ = UnionMembers ()

  taggedUnionNamed name _ _ = SchemaName name

  taggedUnionMemberWithTag _ _ = TaggedUnionMembers ()

  taggedUnionCombine _ _ = TaggedUnionMembers ()

  jsonString schema = schema

