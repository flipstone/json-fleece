{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterBaseResponse
  ( CharacterBaseResponse(..)
  , characterBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CharacterBase (CharacterBase, characterBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data CharacterBaseResponse = CharacterBaseResponse
  { characters :: Maybe [CharacterBase] -- ^ List of characters matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

characterBaseResponseSchema :: FC.Fleece schema => schema CharacterBaseResponse
characterBaseResponseSchema =
  FC.object $
    FC.constructor CharacterBaseResponse
      #+ FC.optional "characters" characters (FC.list characterBaseSchema)
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema