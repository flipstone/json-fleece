{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterRelation
  ( CharacterRelation(..)
  , characterRelationSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.CharacterHeader as CharacterHeader
import qualified StarTrek.CharacterRelation.Type as Type

data CharacterRelation = CharacterRelation
  { type_ :: Maybe Type.Type -- ^ Relation type
  , target :: Maybe CharacterHeader.CharacterHeader -- ^ Header character, embedded in other objects
  , source :: Maybe CharacterHeader.CharacterHeader -- ^ Header character, embedded in other objects
  }
  deriving (Eq, Show)

characterRelationSchema :: FC.Fleece schema => schema CharacterRelation
characterRelationSchema =
  FC.object $
    FC.constructor CharacterRelation
      #+ FC.optional "type" type_ Type.typeSchema
      #+ FC.optional "target" target CharacterHeader.characterHeaderSchema
      #+ FC.optional "source" source CharacterHeader.characterHeaderSchema