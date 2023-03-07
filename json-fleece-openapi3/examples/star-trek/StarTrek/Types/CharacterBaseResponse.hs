{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterBaseResponse
  ( CharacterBaseResponse(..)
  , characterBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CharacterBase as CharacterBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data CharacterBaseResponse = CharacterBaseResponse
  { characters :: Maybe [CharacterBase.CharacterBase] -- ^ Base character, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

characterBaseResponseSchema :: FC.Fleece schema => schema CharacterBaseResponse
characterBaseResponseSchema =
  FC.object $
    FC.constructor CharacterBaseResponse
      #+ FC.optional "characters" characters (FC.list CharacterBase.characterBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema