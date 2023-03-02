{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialBaseResponse
  ( MaterialBaseResponse(..)
  , materialBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.MaterialBase as MaterialBase
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort

data MaterialBaseResponse = MaterialBaseResponse
  { sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , materials :: Maybe [MaterialBase.MaterialBase] -- ^ Base material, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

materialBaseResponseSchema :: FC.Fleece schema => schema MaterialBaseResponse
materialBaseResponseSchema =
  FC.object $
    FC.constructor MaterialBaseResponse
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "materials" materials (FC.list MaterialBase.materialBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema