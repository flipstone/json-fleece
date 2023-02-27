{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialBaseResponse
  ( MaterialBaseResponse(..)
  , materialBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.MaterialBase (MaterialBase, materialBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data MaterialBaseResponse = MaterialBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , materials :: Maybe [MaterialBase] -- ^ List of materials matching given criteria
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

materialBaseResponseSchema :: FC.Fleece schema => schema MaterialBaseResponse
materialBaseResponseSchema =
  FC.object $
    FC.constructor MaterialBaseResponse
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "materials" materials (FC.list materialBaseSchema)
      #+ FC.optional "page" page responsePageSchema