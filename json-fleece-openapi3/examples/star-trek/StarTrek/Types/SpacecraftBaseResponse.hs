{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftBaseResponse
  ( SpacecraftBaseResponse(..)
  , spacecraftBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort
import qualified StarTrek.Types.SpacecraftBase as SpacecraftBase

data SpacecraftBaseResponse = SpacecraftBaseResponse
  { page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , spacecrafts :: Maybe [SpacecraftBase.SpacecraftBase] -- ^ Base spacecraft, returned in search results
  }
  deriving (Eq, Show)

spacecraftBaseResponseSchema :: FC.Fleece t => FC.Schema t SpacecraftBaseResponse
spacecraftBaseResponseSchema =
  FC.object $
    FC.constructor SpacecraftBaseResponse
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "spacecrafts" spacecrafts (FC.list SpacecraftBase.spacecraftBaseSchema)