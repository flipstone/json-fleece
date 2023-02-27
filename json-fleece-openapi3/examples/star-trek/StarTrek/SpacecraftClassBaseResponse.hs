{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftClassBaseResponse
  ( SpacecraftClassBaseResponse(..)
  , spacecraftClassBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.SpacecraftClassBase (SpacecraftClassBase, spacecraftClassBaseSchema)

data SpacecraftClassBaseResponse = SpacecraftClassBaseResponse
  { spacecraftClasses :: Maybe [SpacecraftClassBase] -- ^ List of spacecraft classes matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

spacecraftClassBaseResponseSchema :: FC.Fleece schema => schema SpacecraftClassBaseResponse
spacecraftClassBaseResponseSchema =
  FC.object $
    FC.constructor SpacecraftClassBaseResponse
      #+ FC.optional "spacecraftClasses" spacecraftClasses (FC.list spacecraftClassBaseSchema)
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema