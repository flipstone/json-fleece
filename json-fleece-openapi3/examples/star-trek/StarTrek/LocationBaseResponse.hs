{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationBaseResponse
  ( LocationBaseResponse(..)
  , locationBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.LocationBase (LocationBase, locationBaseSchema)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)

data LocationBaseResponse = LocationBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  , locations :: Maybe [LocationBase] -- ^ List of locations matching given criteria
  }
  deriving (Eq, Show)

locationBaseResponseSchema :: FC.Fleece schema => schema LocationBaseResponse
locationBaseResponseSchema =
  FC.object $
    FC.constructor LocationBaseResponse
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema
      #+ FC.optional "locations" locations (FC.list locationBaseSchema)