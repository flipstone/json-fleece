{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterRelation
  ( CharacterRelation(..)
  , characterRelationSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CharacterHeader (CharacterHeader, characterHeaderSchema)
import StarTrek.CharacterRelation.Type (Type, typeSchema)

data CharacterRelation = CharacterRelation
  { type_ :: Maybe Type -- ^ Relation type
  , target :: Maybe CharacterHeader -- ^ Header character, embedded in other objects
  , source :: Maybe CharacterHeader -- ^ Header character, embedded in other objects
  }
  deriving (Eq, Show)

characterRelationSchema :: FC.Fleece schema => schema CharacterRelation
characterRelationSchema =
  FC.object $
    FC.constructor CharacterRelation
      #+ FC.optional "type" type_ typeSchema
      #+ FC.optional "target" target characterHeaderSchema
      #+ FC.optional "source" source characterHeaderSchema