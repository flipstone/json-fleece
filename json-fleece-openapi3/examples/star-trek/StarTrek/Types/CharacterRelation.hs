{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterRelation
  ( CharacterRelation(..)
  , characterRelationSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CharacterHeader as CharacterHeader
import qualified StarTrek.Types.CharacterRelation.Type as Type

data CharacterRelation = CharacterRelation
  { target :: Maybe CharacterHeader.CharacterHeader -- ^ Header character, embedded in other objects
  , source :: Maybe CharacterHeader.CharacterHeader -- ^ Header character, embedded in other objects
  , type_ :: Maybe Type.Type -- ^ Relation type
  }
  deriving (Eq, Show)

characterRelationSchema :: FC.Fleece schema => schema CharacterRelation
characterRelationSchema =
  FC.object $
    FC.constructor CharacterRelation
      #+ FC.optional "target" target CharacterHeader.characterHeaderSchema
      #+ FC.optional "source" source CharacterHeader.characterHeaderSchema
      #+ FC.optional "type" type_ Type.typeSchema