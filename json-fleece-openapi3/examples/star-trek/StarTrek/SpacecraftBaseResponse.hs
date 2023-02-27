{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftBaseResponse
  ( SpacecraftBaseResponse(..)
  , spacecraftBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.SpacecraftBase (SpacecraftBase, spacecraftBaseSchema)

data SpacecraftBaseResponse = SpacecraftBaseResponse
  { spacecrafts :: Maybe [SpacecraftBase] -- ^ Base spacecraft, returned in search results
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