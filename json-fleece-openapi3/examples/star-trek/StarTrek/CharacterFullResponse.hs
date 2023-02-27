{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterFullResponse
  ( CharacterFullResponse(..)
  , characterFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
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
      #+ FC.optional "character" character characterFullSchema