{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaterialBaseResponse
  ( MaterialBaseResponse(..)
  , materialBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.MaterialBase as MaterialBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data MaterialBaseResponse = MaterialBaseResponse
  { page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , materials :: Maybe [MaterialBase.MaterialBase] -- ^ Base material, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

materialBaseResponseSchema :: FC.Fleece schema => schema MaterialBaseResponse
materialBaseResponseSchema =
  FC.object $
    FC.constructor MaterialBaseResponse
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "materials" materials (FC.list MaterialBase.materialBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema