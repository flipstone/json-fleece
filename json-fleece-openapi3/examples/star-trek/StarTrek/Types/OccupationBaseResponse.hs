{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OccupationBaseResponse
  ( OccupationBaseResponse(..)
  , occupationBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.OccupationBase as OccupationBase
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort

data OccupationBaseResponse = OccupationBaseResponse
  { occupations :: Maybe [OccupationBase.OccupationBase] -- ^ Base occupations, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

occupationBaseResponseSchema :: FC.Fleece t => FC.Schema t OccupationBaseResponse
occupationBaseResponseSchema =
  FC.object $
    FC.constructor OccupationBaseResponse
      #+ FC.optional "occupations" occupations (FC.list OccupationBase.occupationBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema