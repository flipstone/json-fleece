{-# LANGUAGE TypeFamilies #-}

module Fleece.Aeson.EncoderDecoder
  ( EncoderDecoder (..)
  , encoder
  , decoder
  ) where

import Fleece.Aeson.Decoder (Decoder)
import Fleece.Aeson.Encoder (Encoder)
import qualified Fleece.Core as FC

data EncoderDecoder a = EncoderDecoder
  { encoderDecoderEncoder :: Encoder a
  , encoderDecoderDecoder :: Decoder a
  }

encoder :: FC.Schema EncoderDecoder a -> FC.Schema Encoder a
encoder = FC.hoistSchema encoderDecoderEncoder

decoder :: FC.Schema EncoderDecoder a -> FC.Schema Decoder a
decoder = FC.hoistSchema encoderDecoderDecoder

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

  interpretFormat formatString schema =
    EncoderDecoder
      { encoderDecoderEncoder = FC.interpretFormat formatString (encoder schema)
      , encoderDecoderDecoder = FC.interpretFormat formatString (decoder schema)
      }

  interpretNumber name =
    EncoderDecoder
      { encoderDecoderEncoder = FC.interpretNumber name
      , encoderDecoderDecoder = FC.interpretNumber name
      }

  interpretText name =
    EncoderDecoder
      { encoderDecoderEncoder = FC.interpretText name
      , encoderDecoderDecoder = FC.interpretText name
      }

  interpretBoolean name =
    EncoderDecoder
      { encoderDecoderEncoder = FC.interpretBoolean name
      , encoderDecoderDecoder = FC.interpretBoolean name
      }

  interpretArray name itemSchema =
    EncoderDecoder
      { encoderDecoderEncoder = FC.interpretArray name (encoder itemSchema)
      , encoderDecoderDecoder = FC.interpretArray name (decoder itemSchema)
      }

  interpretNull name =
    EncoderDecoder
      { encoderDecoderEncoder = FC.interpretNull name
      , encoderDecoderDecoder = FC.interpretNull name
      }

  interpretNullable name itemSchema =
    EncoderDecoder
      { encoderDecoderEncoder = FC.interpretNullable name (encoder itemSchema)
      , encoderDecoderDecoder = FC.interpretNullable name (decoder itemSchema)
      }

  required name accessor itemSchema =
    Field
      { fieldEncoder = FC.required name accessor (encoder itemSchema)
      , fieldDecoder = FC.required name accessor (decoder itemSchema)
      }

  optional name accessor itemSchema =
    Field
      { fieldEncoder = FC.optional name accessor (encoder itemSchema)
      , fieldDecoder = FC.optional name accessor (decoder itemSchema)
      }

  mapField f fieldEncoderDecoder =
    Field
      { fieldEncoder = FC.mapField f $ fieldEncoder fieldEncoderDecoder
      , fieldDecoder = FC.mapField f $ fieldDecoder fieldEncoderDecoder
      }

  additionalFields accessor itemSchema =
    AdditionalFields
      { additionalFieldsEncoder = FC.additionalFields accessor (encoder itemSchema)
      , additionalFieldsDecoder = FC.additionalFields accessor (decoder itemSchema)
      }

  interpretObjectNamed name objectEncoderDecoder =
    EncoderDecoder
      { encoderDecoderEncoder = FC.interpretObjectNamed name $ objectEncoder objectEncoderDecoder
      , encoderDecoderDecoder = FC.interpretObjectNamed name $ objectDecoder objectEncoderDecoder
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

  interpretValidateNamed name uncheck check itemSchema =
    EncoderDecoder
      { encoderDecoderEncoder = FC.interpretValidateNamed name uncheck check (encoder itemSchema)
      , encoderDecoderDecoder = FC.interpretValidateNamed name uncheck check (decoder itemSchema)
      }

  interpretValidateAnonymous uncheck check itemSchema =
    EncoderDecoder
      { encoderDecoderEncoder = FC.interpretValidateAnonymous uncheck check (encoder itemSchema)
      , encoderDecoderDecoder = FC.interpretValidateAnonymous uncheck check (decoder itemSchema)
      }

  interpretBoundedEnumNamed name toText =
    EncoderDecoder
      { encoderDecoderEncoder = FC.interpretBoundedEnumNamed name toText
      , encoderDecoderDecoder = FC.interpretBoundedEnumNamed name toText
      }

  interpretUnionNamed name members =
    EncoderDecoder
      { encoderDecoderEncoder = FC.interpretUnionNamed name $ unionMembersEncoder members
      , encoderDecoderDecoder = FC.interpretUnionNamed name $ unionMembersDecoder members
      }

  unionMemberWithIndex index itemSchema =
    UnionMembers
      { unionMembersEncoder = FC.unionMemberWithIndex index (encoder itemSchema)
      , unionMembersDecoder = FC.unionMemberWithIndex index (decoder itemSchema)
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

  interpretTaggedUnionNamed name tagProperty members =
    EncoderDecoder
      { encoderDecoderEncoder = FC.interpretTaggedUnionNamed name tagProperty $ taggedUnionMembersEncoder members
      , encoderDecoderDecoder = FC.interpretTaggedUnionNamed name tagProperty $ taggedUnionMembersDecoder members
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

  interpretJsonString itemSchema =
    EncoderDecoder
      { encoderDecoderEncoder = FC.interpretJsonString (encoder itemSchema)
      , encoderDecoderDecoder = FC.interpretJsonString (decoder itemSchema)
      }
