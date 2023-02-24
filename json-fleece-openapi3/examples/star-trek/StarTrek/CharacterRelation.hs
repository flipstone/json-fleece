{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterRelation
  ( CharacterRelation(..)
  , characterRelationSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CharacterHeader (CharacterHeader, characterHeaderSchema)

data CharacterRelation = CharacterRelation
  { type_ :: Maybe Text -- ^ Relation type
  , target :: Maybe CharacterHeader -- ^ Header character, embedded in other objects
  , source :: Maybe CharacterHeader -- ^ Header character, embedded in other objects
  }
  deriving (Eq, Show)

characterRelationSchema :: FC.Fleece schema => schema CharacterRelation
characterRelationSchema =
  FC.object $
    FC.constructor CharacterRelation
      #+ FC.optionalField FC.OmitKey_DelegateNull "type" type_ FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "target" target characterHeaderSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "source" source characterHeaderSchema