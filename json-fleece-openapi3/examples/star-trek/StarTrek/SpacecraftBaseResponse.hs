{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftBaseResponse
  ( SpacecraftBaseResponse(..)
  , spacecraftBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.SpacecraftBase (SpacecraftBase, spacecraftBaseSchema)

data SpacecraftBaseResponse = SpacecraftBaseResponse
  { spacecrafts :: Maybe [SpacecraftBase] -- ^ List of spacecrafts matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

spacecraftBaseResponseSchema :: FC.Fleece schema => schema SpacecraftBaseResponse
spacecraftBaseResponseSchema =
  FC.object $
    FC.constructor SpacecraftBaseResponse
      #+ FC.optional "spacecrafts" spacecrafts (FC.list spacecraftBaseSchema)
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema