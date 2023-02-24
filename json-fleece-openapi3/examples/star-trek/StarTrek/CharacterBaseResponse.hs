{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterBaseResponse
  ( CharacterBaseResponse(..)
  , characterBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "characters" characters (FC.list characterBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema