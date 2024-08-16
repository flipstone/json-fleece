{-# LANGUAGE TypeFamilies #-}

module Fleece.Aeson.EncoderDecoder
  ( EncoderDecoder (..)
  ) where

import Fleece.Aeson.Decoder (Decoder)
import Fleece.Aeson.Encoder (Encoder)
import qualified Fleece.Core as FC

data EncoderDecoder a = EncoderDecoder
  { encoder :: Encoder a
  , decoder :: Decoder a
  }

instance FC.Fleece EncoderDecoder where
  data Object EncoderDecoder object constructor = Object
    { objectEncoder :: FC.Object Encoder object constructor
    , objectDecoder :: FC.Object Decoder object constructor
    }

  data Field EncoderDecoder object a = Field
    { fieldEncoder :: FC.Field Encoder object a
    , fieldDecoder :: FC.Field Decoder object a
    }

  data AdditionalFields EncoderDecoder object a = AdditionalFields
    { additionalFieldsEncoder :: FC.AdditionalFields Encoder object a
    , additionalFieldsDecoder :: FC.AdditionalFields Decoder object a
    }

  data UnionMembers EncoderDecoder allTypes handledTypes = UnionMembers
    { unionMembersEncoder :: FC.UnionMembers Encoder allTypes handledTypes
    , unionMembersDecoder :: FC.UnionMembers Decoder allTypes handledTypes
    }

  data TaggedUnionMembers EncoderDecoder allTags handledTags = TaggedUnionMembers
    { taggedUnionMembersEncoder :: FC.TaggedUnionMembers Encoder allTags handledTags
    , taggedUnionMembersDecoder :: FC.TaggedUnionMembers Decoder allTags handledTags
    }

  type Validator EncoderDecoder = FC.StandardValidator

  schemaName = FC.schemaName . encoder

  number =
    EncoderDecoder
      { encoder = FC.number
      , decoder = FC.number
      }

  text =
    EncoderDecoder
      { encoder = FC.text
      , decoder = FC.text
      }

  boolean =
    EncoderDecoder
      { encoder = FC.boolean
      , decoder = FC.boolean
      }

  array itemEncoderDecoder =
    EncoderDecoder
      { encoder = FC.array $ encoder itemEncoderDecoder
      , decoder = FC.array $ decoder itemEncoderDecoder
      }

  null =
    EncoderDecoder
      { encoder = FC.null
      , decoder = FC.null
      }

  nullable itemEncoderDecoder =
    EncoderDecoder
      { encoder = FC.nullable $ encoder itemEncoderDecoder
      , decoder = FC.nullable $ decoder itemEncoderDecoder
      }

  required name accessor itemEncoderDecoder =
    Field
      { fieldEncoder = FC.required name accessor $ encoder itemEncoderDecoder
      , fieldDecoder = FC.required name accessor $ decoder itemEncoderDecoder
      }

  optional name accessor itemEncoderDecoder =
    Field
      { fieldEncoder = FC.optional name accessor $ encoder itemEncoderDecoder
      , fieldDecoder = FC.optional name accessor $ decoder itemEncoderDecoder
      }

  mapField f fieldEncoderDecoder =
    Field
      { fieldEncoder = FC.mapField f $ fieldEncoder fieldEncoderDecoder
      , fieldDecoder = FC.mapField f $ fieldDecoder fieldEncoderDecoder
      }

  additionalFields accessor itemEncoderDecoder =
    AdditionalFields
      { additionalFieldsEncoder = FC.additionalFields accessor $ encoder itemEncoderDecoder
      , additionalFieldsDecoder = FC.additionalFields accessor $ decoder itemEncoderDecoder
      }

  objectNamed name objectEncoderDecoder =
    EncoderDecoder
      { encoder = FC.objectNamed name $ objectEncoder objectEncoderDecoder
      , decoder = FC.objectNamed name $ objectDecoder objectEncoderDecoder
      }

  constructor f =
    Object
      { objectEncoder = FC.constructor f
      , objectDecoder = FC.constructor f
      }

  field object field =
    Object
      { objectEncoder = FC.field (objectEncoder object) (fieldEncoder field)
      , objectDecoder = FC.field (objectDecoder object) (fieldDecoder field)
      }

  additional object addFields =
    Object
      { objectEncoder =
          FC.additional (objectEncoder object) (additionalFieldsEncoder addFields)
      , objectDecoder =
          FC.additional (objectDecoder object) (additionalFieldsDecoder addFields)
      }

  validateNamed name validator itemEncoderDecoder =
    EncoderDecoder
      { encoder = FC.validateNamed name validator $ encoder itemEncoderDecoder
      , decoder = FC.validateNamed name validator $ decoder itemEncoderDecoder
      }

  boundedEnumNamed name toText =
    EncoderDecoder
      { encoder = FC.boundedEnumNamed name toText
      , decoder = FC.boundedEnumNamed name toText
      }

  unionNamed name members =
    EncoderDecoder
      { encoder = FC.unionNamed name $ unionMembersEncoder members
      , decoder = FC.unionNamed name $ unionMembersDecoder members
      }

  unionMemberWithIndex index itemEncoderDecoder =
    UnionMembers
      { unionMembersEncoder = FC.unionMemberWithIndex index $ encoder itemEncoderDecoder
      , unionMembersDecoder = FC.unionMemberWithIndex index $ decoder itemEncoderDecoder
      }

  unionCombine leftMembers rightMembers =
    UnionMembers
      { unionMembersEncoder =
          FC.unionCombine
            (unionMembersEncoder leftMembers)
            (unionMembersEncoder rightMembers)
      , unionMembersDecoder =
          FC.unionCombine
            (unionMembersDecoder leftMembers)
            (unionMembersDecoder rightMembers)
      }

  taggedUnionNamed name tagProperty members =
    EncoderDecoder
      { encoder = FC.taggedUnionNamed name tagProperty $ taggedUnionMembersEncoder members
      , decoder = FC.taggedUnionNamed name tagProperty $ taggedUnionMembersDecoder members
      }

  taggedUnionMemberWithTag tag membersEncoderDecoder =
    TaggedUnionMembers
      { taggedUnionMembersEncoder = FC.taggedUnionMemberWithTag tag $ objectEncoder membersEncoderDecoder
      , taggedUnionMembersDecoder = FC.taggedUnionMemberWithTag tag $ objectDecoder membersEncoderDecoder
      }

  taggedUnionCombine leftMembers rightMembers =
    TaggedUnionMembers
      { taggedUnionMembersEncoder =
          FC.taggedUnionCombine
            (taggedUnionMembersEncoder leftMembers)
            (taggedUnionMembersEncoder rightMembers)
      , taggedUnionMembersDecoder =
          FC.taggedUnionCombine
            (taggedUnionMembersDecoder leftMembers)
            (taggedUnionMembersDecoder rightMembers)
      }

  jsonString itemEncoderDecoder =
    EncoderDecoder
      { encoder = FC.jsonString $ encoder itemEncoderDecoder
      , decoder = FC.jsonString $ decoder itemEncoderDecoder
      }
