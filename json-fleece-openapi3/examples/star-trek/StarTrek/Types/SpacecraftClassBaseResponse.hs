{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftClassBaseResponse
  ( SpacecraftClassBaseResponse(..)
  , spacecraftClassBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort
import qualified StarTrek.Types.SpacecraftClassBase as SpacecraftClassBase

data SpacecraftClassBaseResponse = SpacecraftClassBaseResponse
  { spacecraftClasses :: Maybe [SpacecraftClassBase.SpacecraftClassBase] -- ^ Base spacecraft class, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

spacecraftClassBaseResponseSchema :: FC.Fleece schema => schema SpacecraftClassBaseResponse
spacecraftClassBaseResponseSchema =
  FC.object $
    FC.constructor SpacecraftClassBaseResponse
      #+ FC.optional "spacecraftClasses" spacecraftClasses (FC.list SpacecraftClassBase.spacecraftClassBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema