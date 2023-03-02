{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationBaseResponse
  ( LocationBaseResponse(..)
  , locationBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.LocationBase as LocationBase
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort

data LocationBaseResponse = LocationBaseResponse
  { sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , locations :: Maybe [LocationBase.LocationBase] -- ^ Base location, returned in search results
  }
  deriving (Eq, Show)

locationBaseResponseSchema :: FC.Fleece schema => schema LocationBaseResponse
locationBaseResponseSchema =
  FC.object $
    FC.constructor LocationBaseResponse
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "locations" locations (FC.list LocationBase.locationBaseSchema)