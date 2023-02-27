{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftClassFullResponse
  ( SpacecraftClassFullResponse(..)
  , spacecraftClassFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.SpacecraftClassFull (SpacecraftClassFull, spacecraftClassFullSchema)

data SpacecraftClassFullResponse = SpacecraftClassFullResponse
  { spacecraftClass :: Maybe SpacecraftClassFull -- ^ Full spacecraft class, returned when queried using UID
  }
  deriving (Eq, Show)

spacecraftClassFullResponseSchema :: FC.Fleece schema => schema SpacecraftClassFullResponse
spacecraftClassFullResponseSchema =
  FC.object $
    FC.constructor SpacecraftClassFullResponse
      #+ FC.optional "spacecraftClass" spacecraftClass spacecraftClassFullSchema