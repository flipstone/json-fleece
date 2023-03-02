{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.OccupationBaseResponse
  ( OccupationBaseResponse(..)
  , occupationBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.OccupationBase as OccupationBase
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort

data OccupationBaseResponse = OccupationBaseResponse
  { occupations :: Maybe [OccupationBase.OccupationBase] -- ^ Base occupations, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

occupationBaseResponseSchema :: FC.Fleece schema => schema OccupationBaseResponse
occupationBaseResponseSchema =
  FC.object $
    FC.constructor OccupationBaseResponse
      #+ FC.optional "occupations" occupations (FC.list OccupationBase.occupationBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema