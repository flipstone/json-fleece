{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftFullResponse
  ( SpacecraftFullResponse(..)
  , spacecraftFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.SpacecraftFull (SpacecraftFull, spacecraftFullSchema)

data SpacecraftFullResponse = SpacecraftFullResponse
  { spacecraft :: Maybe SpacecraftFull -- ^ Full spacecraft, returned when queried using UID
  }
  deriving (Eq, Show)

spacecraftFullResponseSchema :: FC.Fleece schema => schema SpacecraftFullResponse
spacecraftFullResponseSchema =
  FC.object $
    FC.constructor SpacecraftFullResponse
      #+ FC.optional "spacecraft" spacecraft spacecraftFullSchema