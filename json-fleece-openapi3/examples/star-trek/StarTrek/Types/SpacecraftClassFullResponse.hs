{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftClassFullResponse
  ( SpacecraftClassFullResponse(..)
  , spacecraftClassFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.SpacecraftClassFull as SpacecraftClassFull

data SpacecraftClassFullResponse = SpacecraftClassFullResponse
  { spacecraftClass :: Maybe SpacecraftClassFull.SpacecraftClassFull -- ^ Full spacecraft class, returned when queried using UID
  }
  deriving (Eq, Show)

spacecraftClassFullResponseSchema :: FC.Fleece schema => schema SpacecraftClassFullResponse
spacecraftClassFullResponseSchema =
  FC.object $
    FC.constructor SpacecraftClassFullResponse
      #+ FC.optional "spacecraftClass" spacecraftClass SpacecraftClassFull.spacecraftClassFullSchema