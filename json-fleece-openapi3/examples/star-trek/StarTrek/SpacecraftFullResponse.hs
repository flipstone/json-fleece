{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftFullResponse
  ( SpacecraftFullResponse(..)
  , spacecraftFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.SpacecraftFull as SpacecraftFull

data SpacecraftFullResponse = SpacecraftFullResponse
  { spacecraft :: Maybe SpacecraftFull.SpacecraftFull -- ^ Full spacecraft, returned when queried using UID
  }
  deriving (Eq, Show)

spacecraftFullResponseSchema :: FC.Fleece schema => schema SpacecraftFullResponse
spacecraftFullResponseSchema =
  FC.object $
    FC.constructor SpacecraftFullResponse
      #+ FC.optional "spacecraft" spacecraft SpacecraftFull.spacecraftFullSchema