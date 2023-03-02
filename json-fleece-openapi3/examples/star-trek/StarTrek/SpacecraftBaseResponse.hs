{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftBaseResponse
  ( SpacecraftBaseResponse(..)
  , spacecraftBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort
import qualified StarTrek.SpacecraftBase as SpacecraftBase

data SpacecraftBaseResponse = SpacecraftBaseResponse
  { spacecrafts :: Maybe [SpacecraftBase.SpacecraftBase] -- ^ Base spacecraft, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

spacecraftBaseResponseSchema :: FC.Fleece schema => schema SpacecraftBaseResponse
spacecraftBaseResponseSchema =
  FC.object $
    FC.constructor SpacecraftBaseResponse
      #+ FC.optional "spacecrafts" spacecrafts (FC.list SpacecraftBase.spacecraftBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema