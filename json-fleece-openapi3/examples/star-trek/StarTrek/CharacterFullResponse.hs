{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterFullResponse
  ( CharacterFullResponse(..)
  , characterFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CharacterFull (CharacterFull, characterFullSchema)

data CharacterFullResponse = CharacterFullResponse
  { character :: Maybe CharacterFull -- ^ Full character, returned when queried using UID
  }
  deriving (Eq, Show)

characterFullResponseSchema :: FC.Fleece schema => schema CharacterFullResponse
characterFullResponseSchema =
  FC.object $
    FC.constructor CharacterFullResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "character" character characterFullSchema