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
  { page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , spacecraftClasses :: Maybe [SpacecraftClassBase.SpacecraftClassBase] -- ^ Base spacecraft class, returned in search results
  }
  deriving (Eq, Show)

spacecraftClassBaseResponseSchema :: FC.Fleece t => FC.Schema t SpacecraftClassBaseResponse
spacecraftClassBaseResponseSchema =
  FC.object $
    FC.constructor SpacecraftClassBaseResponse
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "spacecraftClasses" spacecraftClasses (FC.list SpacecraftClassBase.spacecraftClassBaseSchema)